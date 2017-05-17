# ########## PLEASE DO NOT MODIFY# ########## #
# Instalacja pakietow i zaladowanie lokalnych plikow *.R
rm(list = ls())
source("infra/instalAndLoadFiles.R")
setupEnvironment()
# ##################################################### #
if(file.exists(getCustomProperty("target"))){
  unlink(getCustomProperty("target"),recursive=TRUE)
}


data.raw <- read.csv(file.path("data", "HR_comma_sep.csv"),sep = ",",header = TRUE);
#automatyczna transformacja wartości nienumerycznych na numeryczne
data <- data_processing.convert_to_numerical_values(data.raw)
### Rozdzielenie na dane wejściowe (cechy) i wyjściowe
data.in <- data[,!(names(data) %in% 'left')]
data.out <- data[,names(data) %in% 'left']

plot <- data_processing.plot_histogram_for_two_clases(data.in[['satisfaction_level']],data.out,0.02,"Liczba wystąpień","Wartość poziomu","Histogram satisfaction_level")
exporter.save_as_png(plot,'satisfaction_level','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['last_evaluation']],data.out,0.02,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'last_evaluation','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['number_project']],data.out,1,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'number_project','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['average_montly_hours']],data.out,5,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'average_montly_hours','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['time_spend_company']],data.out,1,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'time_spend_company','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['Work_accident']],data.out,1,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'Work_accident','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['promotion_last_5years']],data.out,1,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'promotion_last_5years','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['salary']],data.out,1,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'salary','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['sales']],data.out,1,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'sales','histogram')



### Skalowanie i normalizacja
data.in.norm <- data_processing.scale_and_normalize(data.in);
# data.norm zawiera wszystki cechy znormalizowane pomiędzy 0 a 1

### Sprawdzenie korelację pomiędzy cechami
data.in.corr <- cor(data.in.norm)
exporter.save_correlation_as_png(data.in.corr,'all_features','correlation')

### Analiza PCA (Analiza składowych niezależnych)
data.in.pca <- prcomp(data.in.norm,center = FALSE,scale = FALSE,retx=TRUE);

plot <- data_processing.plot_cumulative_pca(data.in.pca)
exporter.save_as_png(plot,'kumulatywne_wyjasnienie_wariancji','pca')

plot<- data_processing.plot_biplot(data.in.pca,data.out)
exporter.save_as_png(plot,'biplot','pca')


data.in.pca.train.folds.indexes <- createFolds(data.out, k = 10, list = TRUE, returnTrain = TRUE)
for (simple.train.fold.indexes in data.in.pca.train.folds.indexes) {
  data.in.train <- data.in.pca$x[simple.train.fold.indexes,]
  data.in.test <- data.in.pca$x[-simple.train.fold.indexes,]
  data.out.train <- data.out[simple.train.fold.indexes]
  data.out.test <- data.out[-simple.train.fold.indexes]
  
  # tutaj dodać różne modele np.:
  # model liniowy
  # model kwadratowy
  # model sieci neuronowej
}




