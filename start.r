# ########## PLEASE DO NOT MODIFY# ########## #
# Instalacja pakietow i zaladowanie lokalnych plikow *.R
rm(list = ls())
source("infra/instalAndLoadFiles.R")
setupEnvironment()
# ##################################################### #
if (file.exists(getCustomProperty("target"))) {
  unlink(getCustomProperty("target"), recursive = TRUE)
}

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
data <-
  transform(data.raw, sales = as.numeric(sales), salary = as.numeric(salary))

### Rozdzielenie na dane wejściowe (cechy) i wyjściowe
column_names <- names(data)
data.in <- data[, !(column_names %in% 'left')]
data.out <- data[, column_names %in% 'left']

data_processing.plot_histogram_for_two_clases(data.in[['sales']],
                                              data.out,
                                              1,
                                              "Liczba wystąpień",
                                              "Poziom sales",
                                              "Histogram sales")

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
data_processing.plot_correlation(data.in.corr)



############################################
### Analiza PCA (Analiza składowych niezależnych)
data.in.pca <-
  prcomp(data.in.norm,
         center = FALSE,
         scale = FALSE,
         retx = TRUE)

n <- colnames(data.in.pca$x)
data.pca.formula <- as.formula(paste("left ~", paste(n[!n %in% "left"], collapse = " + ")))


data_processing.plot_cumulative_pca(data.in.pca)

data_processing.plot_biplot(data.in.pca, data.out)

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
prediction <- round(predict(tree_model, newdata=data.test[, !(column_names %in% 'left')],type ="vector"))

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


nn_model <- neuralnet(formula = data.raw.formula, data=data.norm.train, hidden=c(18),linear.output=FALSE)
# graficzne przedstawienie sieci neuronowej
plot(nn_model)

# predykcja
prediction.raw <- compute(nn_model,data.norm.test[, !(column_names %in% 'left')])$net.result
prediction <- round(prediction.raw)

dataLevels <-min(data.norm.test$left):max(data.norm.test$left)
confusion<-table(factor(prediction, levels=dataLevels),factor(data.norm.test$left, levels=dataLevels))
accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.norm.test)
error <- 1 - accuracy

print(sprintf("Neural network accuracy: %f",accuracy))



# inne modele

#kmeans
klasters <- kmeans(x = data, centers = 2)
Groups <- as.factor(klasters$cluster)
ggplot(data, aes(satisfaction_level, salary, color = Groups)) + geom_point()

# kknn
kknn_model <-
  kknn(
    formula = data.raw.formula,
    train = data,
    test = data,
    k = 10,
    kernel = 'rectangular',
    distance = 1
  )

# random forest
random_forest_model <- randomForest(formula = data.raw.formula,  data)

### PRAKTYCZNA REALIZACJA UCZENIA ALGORYTMU

kFolds = 10;
data.in.pca.train.folds.indexes <-  createFolds(data.out,
                                                k = kFolds,
                                                list = TRUE,
                                                returnTrain = TRUE)

threshold <- 0.5
accuracy.summary=0;
for (simple.train.fold.indexes in data.in.pca.train.folds.indexes) {
 #simple.train.fold.indexes <-data.in.pca.train.folds.indexes$Fold02
    
  
    data.in.train <- as.data.frame(data.in.pca$x[simple.train.fold.indexes, ])
    data.in.test <- as.data.frame(data.in.pca$x[-simple.train.fold.indexes, ])
    data.out.train <- data.out[simple.train.fold.indexes]
    data.out.test <- data.out[-simple.train.fold.indexes]
  
  #dodanie kolumny 
  data.in.train$left <- data.out.train
  
  tree_model <- rpart(formula = data.pca.formula,  data = data.in.train)
  
  prediction <- round(predict(tree_model, newdata= data.in.test))
  dataLevels <-min(data.out.test):max(data.out.test)
  confusion <- table(factor(prediction, levels=dataLevels),factor(data.out.test, levels=dataLevels))
  accuracy <- (confusion[1,1]+confusion[2,2])/nrow(data.in.test)
  print(accuracy)
  accuracy.summary <- accuracy.summary+accuracy
  error <- 1 - accuracy
  
}
accuracy.summary <- accuracy.summary/kFolds
print(accuracy.summary)
