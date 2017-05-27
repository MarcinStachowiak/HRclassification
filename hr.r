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
data.raw <- read.csv(file.path("data", "HR_comma_sep.csv"), sep = ",", header = TRUE)

########################################################
# ANALIZA DANYCH
########################################################
str(data.raw) # struktura danych
print(data.raw) # wypisanie danych
summary(data.raw) # podsumowanie kolumn

########################################################
# ZMIANA KOLEJNOŚCI KOLUMN
########################################################
data.raw = data.raw[c(1:6,8:10,7)]

########################################################
# RYSOWANIE HISTOGRAMU DLA WYBRANEJ CECHY Z PODZIAŁEM NA KLASY
########################################################
par(mfrow=c(2,1))
n <- 2 # numer cechy
hist(data.raw[data.raw$left==0, n],main = paste("Rozkład ", names(data.raw)[n]), ylab = "Wystąpień", xlab="Wartość cechy")  #  histogram pierwszej cechy
hist(data.raw[data.raw$left==1, n],main = paste("Rozkład ", names(data.raw)[n]), ylab = "Wystąpień", xlab="Wartość cechy")  #  histogram drugiej cechy

########################################################
# FORMUŁA
########################################################
data.raw.formula <- as.formula(paste("left ~", paste(names(data.raw[1:9]), collapse = " + ")))

########################################################
# ANALIZA WPŁYWU CECH
########################################################
par(mfrow=c(1,1))
attr_importance <- information.gain(data.raw.formula, data.raw)
attr_names <- row.names(attr_importance)
barplot(
  height = attr_importance[[1]],
  main = 'Ważność cechy',
  names.arg = attr_names,
  col = rainbow(length(t(attr_names))),
  legend.text = attr_names
)

########################################################
# TRANSFORMACJA WARTOŚCI NIENUMERWYCZNYCH NA NUMERYCZNE
########################################################
data.in <- transform(data.raw, sales = as.numeric(sales), salary = as.numeric(salary))

########################################################
# SKALOWANIE I NORMALIZACJA CECH WEJŚCIOWYCH
########################################################
data.in.norm <-
  as.data.frame(lapply(data.in,  function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }))

########################################################
# BADANIE KORELACJI POMĘDZY CECHAMI
########################################################
data.in.corr <- cor(data.in.norm[1:9])

########################################################
# RYSOWANIE WYKRESU KORELACJI
########################################################
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
p <- corrplot(data.in.corr, method="color", col=col(200),  
              type="upper", order="hclust", 
              addCoef.col = "black",
              tl.col="black",
              tl.srt=30,
              diag=T)

########################################################
# ANALIZA SKŁADOWYCH NIEZALEŻNYCH
########################################################
data.in.pca <- prcomp(data.in.norm[1:9], center = FALSE, scale = FALSE, retx = TRUE)
data.in.pca.data <- data.frame(cbind(data.in.pca$x, left = data.in.norm$left))
data.pca.formula <- as.formula(paste("left ~", paste(colnames(data.in.pca.data[,1:9]), collapse = " + ")))
########################################################
#RYSOWANIE WYKRESU KUMULATYWNEJ PROPORCJI WYJAŚNIENIA WARIANCJI CECH
########################################################
data.in.pca.sdev <- data.in.pca$sdev
data.in.pca.stddev <- data.in.pca.sdev^2
data.in.pca.stddev.prop <- data.in.pca.stddev/sum(data.in.pca.stddev)

df <- data.frame(data_y=cumsum(data.in.pca.stddev.prop), data_x=colnames(data.in.pca$rotation))

ggplot(df, aes(data_x, data_y, group=1))+
  geom_line(colour="red") + 
  geom_point(size=3, fill="white")+
  ylim(0,1)+
  geom_text(size = 3, position = position_stack(vjust =0.97),label=round(cumsum(data.in.pca.stddev.prop),digits=2))+
  labs(title='Kumulatywna proporcja wyjasnianej wariancji cech wejściowych przez niezalezne komponenty',x='Niezalezne komponenty', y = 'Kumulatywna proporcja wyjasnionej wariancji')

########################################################
# NARYSOWANIE WYKRESU BIPLOT
########################################################
ggbiplot(data.in.pca , obs.scale = 1, var.scale = 1, 
         groups = data.in[,10], ellipse = TRUE, 
         circle = TRUE) +
  scale_color_continuous(name = '') + 
  theme(legend.direction = 'horizontal', legend.position = 'top')


########################################################
# METODA POMOCNICZA
########################################################
predictMe <- function(modelName, formula, data, testData, trControl){
  model <- train(formula, method=modelName, data=data, trControl= TC)
  predictCrossVal <- predict(model, testData)
  conf <- confusionMatrix(testData$left, predictCrossVal)
  print(paste(modelName, sprintf(" accuracy: %f", conf$overall["Accuracy"])))
  list(model=model,
       prediction=predictCrossVal,
       overall=conf$overall,
       confusion=conf$table)
}

########################################################
# PRZYGOTOWANIE DANYCH TESTOWYCH I TRENINGOWYCH A TAKŻE KFOLDA
########################################################
splitSample <- sample(1:2, size=nrow(data.in.norm), prob=c(0.7,0.3), replace=TRUE)
data.norm.train <- data.in.norm[splitSample==1,]
data.norm.test <- data.in.norm[splitSample==2,]

data.norm.train$left<-as.factor(data.norm.train$left)
TC <- trainControl(method = "cv", number = 12, returnData=FALSE, returnResamp="none", savePredictions=FALSE, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)

########################################################
# nnet
########################################################
nnet_prediction <- predictMe("nnet", data.raw.formula, data.norm.train, data.norm.test, TC)
plotnet(nnet_prediction$model, node_labs = TRUE, var_labs = TRUE)
########################################################
# bayesglm
########################################################
bayesglm_prediction <- predictMe("bayesglm", data.raw.formula, data.norm.train, data.norm.test, TC)
########################################################
# gbm
########################################################
gbm_prediction <- predictMe("gbm", data.raw.formula, data.norm.train, data.norm.test, TC)
plot(gbm_prediction$model) # Accuracy względem iteracji i głębokości drzewa
########################################################
# knn
########################################################
knn_prediction <- predictMe("knn", data.raw.formula, data.norm.train, data.norm.test, TC)
plot(knn_prediction$model) # Acuuracy wzklędem ilości sąsiadów
########################################################
# nb
########################################################
nb_prediction <- predictMe("nb", data.raw.formula, data.norm.train, data.norm.test, TC)
########################################################
# rf
########################################################
rf_prediction <- predictMe("rf", data.raw.formula, data.norm.train, data.norm.test, TC)
plot(rf_prediction$model) # Accuracy względem predyktorów
########################################################
# rpart
########################################################
rpart_prediction <- predictMe("rpart", data.raw.formula, data.norm.train, data.norm.test, TC)
########################################################
# svmLinear
########################################################
svmLinear_prediction <- predictMe("svmLinear", data.raw.formula, data.norm.train, data.norm.test, TC)

########################################################
# svmRadial
########################################################
svmRadial_prediction <- predictMe("svmRadial", data.raw.formula, data.norm.train, data.norm.test, TC)
########################################################
# treebag
########################################################
treebag_prediction <- predictMe("treebag", data.raw.formula, data.norm.train, data.norm.test, TC)

trainMethods <- c("Bayesian GLM", "Generalized Boosted Regression",
                  "K Nearest Neighbor",
                  "Naive Bayes",
                  "Neural Net",
                  "Random Forest",
                  "Recursive Partitioning and Regression Trees",
                  "Support Vector Machines Linear",
                  "Support Vector Machines Radial",
                  "Bagged Classification and Regression Trees")
accuracy <- c(nnet_prediction$overall["Accuracy"], 
              bayesglm_prediction$overall["Accuracy"],  
              gbm_prediction$overall["Accuracy"], 
              knn_prediction$overall["Accuracy"], 
              nb_prediction$overall["Accuracy"], 
              rf_prediction$overall["Accuracy"], 
              rpart_prediction$overall["Accuracy"], 
              svmLinear_prediction$overall["Accuracy"], 
              svmRadial_prediction$overall["Accuracy"], 
              treebag_prediction$overall["Accuracy"])
error <- 1 - accuracy

summary <- data.frame(metoda=trainMethods, accuracy, error)
summary[order(summary$accuracy),]