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

#st=stat.desc(satisfaction_level_data)


plot <- data_processing.plot_histogram_for_two_clases(data.in[['satisfaction_level']],data.out,0.02,"Liczba wystąpień","Wartość poziomu","Histogram satisfaction_level")
exporter.save_as_png(plot,'satisfaction_level','histogram')

plot <- data_processing.plot_histogram_for_two_clases(data.in[['last_evaluation']],data.out,0.02,"Liczba wystąpień","Wartość poziomu","Histogram last_evaluation")
exporter.save_as_png(plot,'last_evaluation','histogram')

plot <- data_procebng(plot,'Work_accident','histogram')


### Skalowanie i normalizacja
data.in.norm <- data_processing.scale_and_normalize(data.in);
# data.norm zawiera wszystki cechy znormalizowane pomiędzy 0 a 1

### Sprawdzenie korelację pomiędzy cechami
data.in.corr <- cor(data.in.norm)
exporter.save_correlation_as_png(data.in.corr,'all_features','correlation')

### Analiza PCA (Analiza składowych niezależnych)
data.in.pca <- prcomp(data.in.norm,center = FALSE,scale = FALSE);

plot <- data_processing.plot_cumulative_pca(data.in.pca)
exporter.save_as_png(plot,'kumulatywne_wyjasnienie_wariancji','pca')