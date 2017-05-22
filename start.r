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


############################################
# Załadowanie danych z pliku
data.raw <- read.csv(file.path("data", "HR_comma_sep.csv"),
                     sep = ",",
                     header = TRUE)


############################################
# Analiza danych
str(data.raw)
print(data.raw)
summary(data.raw)


# analiza wplywu cech
n <- names(data.raw)
data.raw.formula <- as.formula(paste("left ~", paste(n[!n %in% "left"], collapse = " + ")))
attr_importance <- information.gain(data.raw.formula, data.raw)
column_names <- names(data.raw)
attr_names <- names(data.raw[, !(column_names %in% 'left')])
barplot(
  height = attr_importance[[1]],
  main = 'Information gain',
  names.arg = attr_names,
  col = rainbow(length(t(attr_names))),
  legend.text = attr_names
)


############################################
# Transformacja wartości nienumerycznych na numeryczne
data <- transform(data.raw, sales = as.numeric(sales), salary = as.numeric(salary))

### Rozdzielenie na dane wejściowe (cechy) i wyjściowe
column_names <- names(data)
data.in <- data[, !(column_names %in% 'left')]
data.out <- data[, column_names %in% 'left']

# Rysowanie histogramu dla konkretnej cechy i dwóch klas
df <- data.frame(
  data_output=factor(data.out), 
  data_input=data.in[['sales']]
)
mu <- ddply(df, "data_output", summarise, grp.mean=mean(data_input))
ggplot(
  df, 
  aes(x=data_input,fill=data_output,color=data_output)) + 
  geom_histogram(binwidth =2,alpha=0.5,position="identity") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=data_output),linetype="dashed", size=1,show.legend = F) + 
  labs(title="Histogram sales",x="Poziom sales", y = "Liczba wystąpień") +
  theme(plot.title = element_text(hjust = 0.5))

### Skalowanie i normalizacja danych wejściowych
data.in.norm <-
  as.data.frame(lapply(data.in,  function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }))

### Skalowanie i normalizacja wszystkich danych (potrzebne w niektórych modelach)
data.norm <-
  as.data.frame(lapply(data,  function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }))



############################################
### Sprawdzenie korelację pomiędzy cechami
data.in.corr <- cor(data.in.norm)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# Rysowanie wykresu korelacji
p <- corrplot(data.in.corr, method="color", col=col(200),  
              type="upper", order="hclust", 
              addCoef.col = "black", # Add coefficient of correlation
              tl.col="black", tl.srt=45, #Text label color and rotationcorrplot
              diag=T)




############################################
### Analiza PCA (Analiza składowych niezależnych)
data.in.pca <-
  prcomp(data.in.norm,
         center = FALSE,
         scale = FALSE,
         retx = TRUE)

n <- colnames(data.in.pca$x)
data.pca.formula <- as.formula(paste("left ~", paste(n[!n %in% "left"], collapse = " + ")))

# Rysowanie wykresu kumulatywnej proporcji wyjaśnienia wariancji cech
data.in.pca.sdev <- data.in.pca$sdev
data.in.pca.stddev <- data.in.pca.sdev^2
data.in.pca.stddev.prop <- data.in.pca.stddev/sum(data.in.pca.stddev)

df <- data.frame(data_y=cumsum(data.in.pca.stddev.prop),
                 data_x=colnames(data.in.pca$rotation))

ggplot(df, aes(data_x,data_y,group=1))+
  geom_line(colour="red") + 
  geom_point(size=3, fill="white")+
  ylim(0,1)+
  geom_text(size = 3, position = position_stack(vjust =0.97),label=round(cumsum(data.in.pca.stddev.prop),digits=2))+
  labs(title='Kumulatywna proporcja wyjasnianej wariancji cech wejściowych przez niezalezne komponenty',x='Niezalezne komponenty', y = 'Kumulatywna proporcja wyjasnionej wariancji')


# Narysowanie wykresu typu biplot dla cech wejściowch i zbudowanych na ich podstawie niezaleznych komponentów
ggbiplot(data.in.pca , obs.scale = 1, var.scale = 1, 
         groups = data.out, ellipse = TRUE, 
         circle = TRUE) +
  scale_color_continuous(name = '') + 
  theme(legend.direction = 'horizontal', legend.position = 'top')

### TESTOWANIE MODELI UCZĄCYCH NA SUROWYCH DANYCH (bez obróbki PCA)
# Przygotowanie danych treningowych
splitSample <- sample(1:2, size=nrow(data.raw),prob=c(0.7,0.3), replace=TRUE)
data.train <- data.raw[splitSample==1,]
data.test <- data.raw[splitSample==2,]

# Stworzenie modelu Drzewa
tree_model <- rpart(formula = data.raw.formula,  data = data.train)

rpart.plot(
  tree_model ,
  box.palette = "GnBu",
  branch.lty = 3,
  shadow.col = "gray",
  nn = TRUE
)

# predykcja
prediction <- round(predict(tree_model, newdata=data.test[, !(column_names %in% 'left')]))

# nie zawsze działa
confusionMatrix(prediction, data.test$left)

# ale za to można zrobić macierz manualnie
dataLevels <-min(data.test$left):max(data.test$left)
confusion<-table(factor(prediction, levels=dataLevels),factor(data.test$left, levels=dataLevels))

accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.test)
error <- 1 - accuracy
print(sprintf("Tree accuracy: %f",accuracy))


# sieć neuronowa (MLP) - na danych znormalizowanych
splitSample <- sample(1:2, size=nrow(data.norm),prob=c(0.7,0.3), replace=TRUE)
data.norm.train <- data.norm[splitSample==1,]
data.norm.test <- data.norm[splitSample==2,]

nn_model <- neuralnet(formula = data.raw.formula, data=data.norm.train, hidden=c(8),linear.output=FALSE)
# graficzne przedstawienie sieci neuronowej
plot(nn_model)

# predykcja
prediction.raw <- compute(nn_model, data.norm.test[, !(column_names %in% 'left')])$net.result
prediction <- round(prediction.raw)

dataLevels <-min(data.norm.test$left):max(data.norm.test$left)
confusion<-table(factor(prediction, levels=dataLevels),factor(data.norm.test$left, levels=dataLevels))
accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.norm.test)
error <- 1 - accuracy

print(sprintf("Neural network accuracy: %f",accuracy))


#### inne modele
#kmeans
klasters <- kmeans(x = data.in[,1:2], centers = 2)
klasters.groups <- as.factor(klasters$cluster)
ggplot(data.in, aes(satisfaction_level, last_evaluation, color = klasters.groups)) + geom_point()


# kknn
knn_model <-
  knn(
    cl = data.out[splitSample==1],
    train = data.in.pca$x[splitSample==1, ],
    test = data.in.pca$x[splitSample==2, ],
    k = 1
  )

CrossTable(x=data.out[splitSample==2], y=knn_model, prop.chisq = FALSE, prop.t=FALSE, prop.c=FALSE, prop.r=FALSE)

# model liniowy 
lm_model <- lm(formula = data.raw.formula, data = data.train)

plot_data_predicted <- data.frame(x=1:nrow(data.test), y=as.data.frame(prediction)[[1]])
plot_data_predicted$c <- plot_data_predicted$x * plot_data_predicted$y
plot_data_real <- data.frame(x=1:nrow(data.test),y=data.test$left)
plot_data_real$c <- plot_data_real$x * plot_data_real$y

ggplot(plot_data_predicted, aes(x=plot_data_predicted$c,y=plot_data_real$c))+
  geom_point(shape=1, colour = "blue")

# random forest
random_forest_model <- randomForest(formula = data.raw.formula,  data.train)

### PRAKTYCZNA REALIZACJA UCZENIA ALGORYTMU

kFolds = 10;
data.in.pca.train.folds.indexes <-  createFolds(data.out,
                                                k = kFolds,
                                                list = TRUE,
                                                returnTrain = TRUE)

accuracy.summary=0;
for (simple.train.fold.indexes in data.in.pca.train.folds.indexes) {
    data.in.train <- as.data.frame(data.in.pca$x[simple.train.fold.indexes, ])
    data.in.test <- as.data.frame(data.in.pca$x[-simple.train.fold.indexes, ])
    data.out.train <- data.out[simple.train.fold.indexes]
    data.out.test <- data.out[-simple.train.fold.indexes]
  
  #dodanie kolumny 
  data.in.train$left <- data.out.train
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
