rm(list = ls())
########################################################
# WCZYTYWANIE BIBLIOTEK
########################################################
library(ggplot2)
library(plyr)
library(corrplot)
library(digest)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(class)
library(gmodels)
library(FSelector)
library(neuralnet)
library(e1071)
library(ggbiplot)


########################################################
# ZAŁADOWANIE DANYCH Z PLIKU
########################################################
data.raw <- read.csv(file.path("data", "data_cancer.csv"),
                     sep = ",",
                     header = TRUE)


########################################################
# ANALIZA DANYCH
########################################################
str(data.raw)
print(data.raw)
summary(data.raw)


########################################################
# TRANSFORMACJA WARTOŚCI NIENUMERWYCZNYCH NA NUMERYCZNE
########################################################
#brak wartości nienumerycznych dla zbioru  data_cancer.scv
data.raw <- transform(data.raw, diagnosis = as.numeric(diagnosis))


########################################################
# ROZDZIELENIE NA DANE (CECHY) WEJŚCIOWE I WYJŚCIOWE
########################################################
column_names <- names(data.raw)
data.in <- data.raw[, !(column_names %in% 'diagnosis')]
data.out <- data.raw[, column_names %in% 'diagnosis']

########################################################
# RYSOWANIE HISTOGRAMU DLA WYBRANEJ CECHY Z PODZIAŁEM NA KLASY
########################################################
df <- data.frame(
  data_output=factor(data.out), 
  data_input=data.in[['area_mean']]
)
mu <- ddply(df, "data_output", summarise, grp.mean=mean(data_input))
ggplot(
  df, 
  aes(x=data_input,fill=data_output,color=data_output)) + 
  geom_histogram(binwidth =50,alpha=0.5,position="identity") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=data_output),linetype="dashed", size=1,show.legend = F) + 
  labs(title="Histogram area_mean",x="Średnia wielkość", y = "Liczba wystąpień") +
  theme(plot.title = element_text(hjust = 0.5))

########################################################
# ANALIZA WPŁYWU CECH
########################################################
n <- names(data.raw)
data.raw.formula <- as.formula(paste("diagnosis ~", paste(n[!n %in% "diagnosis"], collapse = " + ")))
attr_importance <- information.gain(data.raw.formula, data.raw)
column_names <- names(data.raw)
attr_names <- names(data.raw[, !(column_names %in% 'diagnosis')])
barplot(
  height = attr_importance[[1]],
  main = 'Information gain',
  names.arg = attr_names,
  col = rainbow(length(t(attr_names))),
  legend.text = attr_names
)


########################################################
# SKALOWANIE I NORMALIZACJA CECH WEJŚCIOWYCH
########################################################
data.in.norm <-
  as.data.frame(lapply(data.in,  function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }))

data.raw.norm <-
  as.data.frame(lapply(data.raw,  function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }))



########################################################
# BADANIE KORELACJI POMĘDZY CECHAMI
########################################################
data.in.corr <- cor(data.in.norm)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

########################################################
# RYSOWANIE WYKRESU KORELACJI
########################################################
p <- corrplot(data.in.corr, method="color", col=col(200),  
              type="upper", order="hclust", , tl.cex = 1/par("cex"),
              addCoef.col = "black", # Add coefficient of correlation
              tl.col="black", tl.srt=40, #Text label color and rotationcorrplot
              diag=T,number.cex=0.5)




########################################################
# ANALIZA SKŁADOWYCH NIEZALEŻNYCH
########################################################
data.in.pca <-
  prcomp(data.in.norm,
         center = FALSE,
         scale = FALSE,
         retx = TRUE)


########################################################
#RYSOWANIE WYKRESU KUMULATYWNEJ PROPORCJI WYJAŚNIENIA WARIANCJI CECH
########################################################
data.in.pca.sdev <- data.in.pca$sdev
data.in.pca.stddev <- data.in.pca.sdev^2
data.in.pca.stddev.prop <- data.in.pca.stddev/sum(data.in.pca.stddev)

df <- data.frame(data_y=cumsum(data.in.pca.stddev.prop),
                 data_x=seq(1,length(colnames(data.in.pca$rotation))))

ggplot(df, aes(x=data_x,y=data_y,group=1))+
  geom_line(colour="red") + 
  geom_point(size=3, fill="white")+
  ylim(0,1)+
  geom_text(size = 3, position = position_stack(vjust =0.97),label=round(cumsum(data.in.pca.stddev.prop),digits=2))+
  geom_text(size = 3, position = position_stack(vjust =0.94),label=colnames(data.in.pca$rotation))+
  labs(title='Kumulatywna proporcja wyjasnianej wariancji cech wejściowych przez niezalezne komponenty',x='Niezalezne komponenty', y = 'Kumulatywna proporcja wyjasnionej wariancji')


########################################################
# NARYSOWANIE WYKRESU BIPLOT
########################################################
ggbiplot(data.in.pca , obs.scale = 1, var.scale = 1, 
         groups = data.out, ellipse = TRUE, 
         circle = TRUE) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

########################################################
# TESTOWANIE MODELI UCZĄCYCH NA SUROWYCH DANYCH (bez PCA)

# DRZEWA DECYZYJNE (bez PCA)
########################################################
# Przygotowanie danych treningowych
splitSample <- sample(1:2, size=nrow(data.raw),prob=c(0.7,0.3), replace=TRUE)
data.raw.train <- data.raw[splitSample==1,]
data.raw.test <- data.raw[splitSample==2,]

# Stworzenie modelu Drzewa
tree_model <- rpart(formula = data.raw.formula,  data = data.raw.train)
rpart.plot(
  tree_model ,
  box.palette = "GnBu",
  branch.lty = 3,
  shadow.col = "gray",
  nn = TRUE
)

# predykcja
prediction <- round(predict(tree_model, newdata=data.raw.test[, !(column_names %in% 'diagnosis')]))

# macierz współwystępowania
dataLevels <-min(data.raw.test$diagnosis):max(data.raw.test$diagnosis)
confusion<-table(factor(prediction, levels=dataLevels),factor(data.raw.test$diagnosis, levels=dataLevels))

accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.raw.test)
error <- 1 - accuracy
print(sprintf("Tree accuracy: %f",accuracy))


########################################################
# SIEĆ NEURONOWAE (bez PCA)
########################################################
nn_model <- neuralnet(formula = data.raw.formula, data=data.norm.train, hidden=c(30, 6),linear.output=FALSE)
# graficzne przedstawienie sieci neuronowej
plot(nn_model)

# predykcja
prediction.raw <- compute(nn_model, data.norm.test[, !(column_names %in% 'diagnosis')])$net.result
prediction <- round(prediction.raw)

dataLevels <-min(data.norm.test$diagnosis):max(data.norm.test$diagnosis)
confusion<-table(factor(prediction, levels=dataLevels),factor(data.norm.test$diagnosis, levels=dataLevels))
accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.norm.test)
error <- 1 - accuracy

print(sprintf("Neural network accuracy: %f",accuracy))





########################################################
# TESTOWANIE MODELI UCZĄCYCH NA SUROWYCH DANYCH (z PCA)

# DRZEWA DECYZYJNE (z PCA)
########################################################
# Przygotowanie danych treningowych
number.of.pca=10  # liczba komponentów pca wziętych do klasyfikacji
n <- colnames(data.in.pca$x[,1:number.of.pca])
data.pca.formula <- as.formula(paste("diagnosis ~", paste(n[!n %in% "diagnosis"], collapse = " + ")))


data.in.out.pca <- data.frame(cbind(data.in.pca$x[,1:number.of.pca], diagnosis = data.raw$diagnosis))

splitSample <- sample(1:2, size=nrow(data.in.out.pca),prob=c(0.7,0.3), replace=TRUE)
data.in.out.pca.train <- data.in.out.pca[splitSample==1,]
data.in.out.pca.test <- data.in.out.pca[splitSample==2,]

# Stworzenie modelu Drzewa
tree_model <- rpart(formula = data.pca.formula,  data = data.in.out.pca.train)
rpart.plot(
  tree_model,
  box.palette = "GnBu",
  branch.lty = 3,
  shadow.col = "gray",
  nn = TRUE
)

# predykcja
prediction <- round(predict(tree_model, newdata=data.in.out.pca.test[, n]))

# macierz współwystępowania
dataLevels <-min(data.in.out.pca.test$diagnosis):max(data.in.out.pca.test$diagnosis)
confusion<-table(factor(prediction, levels=dataLevels),factor(data.in.out.pca.test$diagnosis, levels=dataLevels))

accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.in.out.pca.test)
error <- 1 - accuracy
print(sprintf("Tree accuracy: %f",accuracy))


########################################################
# SIEĆ NEURONOWAE (z PCA)
########################################################
nn_model <- neuralnet(formula = data.pca.formula, data=data.in.out.pca.train, hidden=c(30, 6),linear.output=FALSE)
# graficzne przedstawienie sieci neuronowej
plot(nn_model)

# predykcja
prediction.raw <- compute(nn_model, data.in.out.pca.test[, n])$net.result
prediction <- round(prediction.raw)

dataLevels <-min(data.in.out.pca.test$diagnosis):max(data.in.out.pca.test$diagnosis)
confusion<-table(factor(prediction, levels=dataLevels),factor(data.in.out.pca.test$diagnosis, levels=dataLevels))
accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.in.out.pca.test)
error <- 1 - accuracy

print(sprintf("Neural network accuracy: %f",accuracy))





########################################################
# PRAKTYCZNA REALIZACJA UCZENIA ALGORYTMU
# WALIDACJA KRZYŻOWA
########################################################
kFolds = 10;
data.in.pca.train.folds.indexes <-  createFolds(data.out,
                                                k = kFolds,
                                                list = TRUE,
                                                returnTrain = TRUE)

accuracy.summary=0;
########################################################
# ZASTOSOWANIE DOWOLNEGO ALGORYTMU KLASYFIKUJĄCEGO NA K ZBIORACH DANYCH
########################################################
for (simple.train.fold.indexes in data.in.pca.train.folds.indexes) {
  data.in.train <- as.data.frame(data.in.pca$x[simple.train.fold.indexes, ])
  data.in.test <- as.data.frame(data.in.pca$x[-simple.train.fold.indexes, ])
  data.out.train <- data.out[simple.train.fold.indexes]
  data.out.test <- data.out[-simple.train.fold.indexes]
  
  #dodanie kolumny 
  data.in.train$diagnosis <- data.out.train
  # zbudowanie modelu
  tree_model <- rpart(formula = data.pca.formula,  data = data.in.train)
  # predykcja
  prediction <- round(predict(tree_model, newdata= data.in.test))
  
  # obliczenie confusionMatrix
  dataLevels <-min(data.out.test):max(data.out.test)
  confusion <- table(factor(prediction, levels=dataLevels),factor(data.out.test, levels=dataLevels))
  # wyznaczenie skuteczności klasyfikacji
  accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.in.test)
  print(accuracy)
  accuracy.summary <- accuracy.summary+accuracy
  error <- 1 - accuracy
  
}
accuracy.summary <- accuracy.summary/kFolds
print(accuracy.summary)
