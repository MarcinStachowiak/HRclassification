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

data.formula <-  formula('left ~ .')
attr_importance <- information.gain(data.formula, data.raw)
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

### Skalowanie i normalizacja
data.in.norm <-
  as.data.frame(lapply(data.in,  function(x) {
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


data_processing.plot_cumulative_pca(data.in.pca)

data_processing.plot_biplot(data.in.pca, data.out)

data.in.pca.train.folds.indexes <-  createFolds(data.out,
                                                k = 10,
                                                list = TRUE,
                                                returnTrain = TRUE)
#for (simple.train.fold.indexes in data.in.pca.train.folds.indexes) {
simple.train.fold.indexes = data.in.pca.train.folds.indexes[[1]]
data.in.train <- data.in.pca$x[simple.train.fold.indexes, ]
data.in.test <- data.in.pca$x[-simple.train.fold.indexes, ]
data.out.train <- data.out[simple.train.fold.indexes]
data.out.test <- data.out[-simple.train.fold.indexes]

# tutaj dodać różne modele np.:
# model liniowy
# model kwadratowy
# model sieci neuronowej
#}

tree_model <- rpart(formula = data.formula,  data = data)

rpart.plot(
  tree_model ,
  box.palette = "GnBu",
  branch.lty = 3,
  shadow.col = "gray",
  nn = TRUE
)


kknn_model <-
  kknn(
    formula = data.formula,
    train = data,
    test = data,
    k = 10,
    kernel = 'rectangular',
    distance = 1
  )

random_forest_model <- randomForest(formula = data.formula,  data)


predict(random_forest_model, data[1:3, !(column_names %in% 'left')])

klasters <- kmeans(x = data,
                   centers = 2)
Groups <- as.factor(klasters$cluster)
ggplot(data, aes(satisfaction_level, salary, color = Groups)) + geom_point()