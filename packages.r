if (!is.element('caret', installed.packages()[, 1])) {
  install.packages('caret', repos = "http://cran.rstudio.com/")
}
if (!is.element('ggplot2', installed.packages()[, 1])) {
  install.packages('ggplot2', repos = "http://cran.rstudio.com/")
}
if (!is.element('devtools', installed.packages()[, 1])) {
  install.packages('devtools', repos = "http://cran.rstudio.com/")
}
if (!is.element('corrplot', installed.packages()[, 1])) {
  install.packages('corrplot', repos = "http://cran.rstudio.com/")
}
if (!is.element('FSelector', installed.packages()[, 1])) {
  install.packages('FSelector', repos = "http://cran.rstudio.com/")
}
library(devtools)
if (!is.element('ggbiplot', installed.packages()[, 1])) {
  install_github('ggbiplot', 'vqv')
}
if (!is.element('NeuralNetTools', installed.packages()[, 1])) {
  install.packages('NeuralNetTools', repos = "http://cran.rstudio.com/")
}