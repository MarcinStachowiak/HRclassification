if (!is.element('ggplot2', installed.packages()[, 1])) {
  install.packages('ggplot2', repos = "http://cran.rstudio.com/")
}
if (!is.element('plyr', installed.packages()[, 1])) {
  install.packages('plyr', repos = "http://cran.rstudio.com/")
}
if (!is.element('corrplot', installed.packages()[, 1])) {
  install.packages('corrplot', repos = "http://cran.rstudio.com/")
}
if (!is.element('digest', installed.packages()[, 1])) {
  install.packages('digest', repos = "http://cran.rstudio.com/")
}
if (!is.element('caret', installed.packages()[, 1])) {
  install.packages('caret', repos = "http://cran.rstudio.com/")
}
if (!is.element('rpart', installed.packages()[, 1])) {
  install.packages('rpart', repos = "http://cran.rstudio.com/")
}
if (!is.element('rpart.plot', installed.packages()[, 1])) {
  install.packages('rpart.plot', repos = "http://cran.rstudio.com/")
}
if (!is.element('randomForest', installed.packages()[, 1])) {
  install.packages('randomForest', repos = "http://cran.rstudio.com/")
}
if (!is.element('class', installed.packages()[, 1])) {
  install.packages('class', repos = "http://cran.rstudio.com/")
}
if (!is.element('gmodels', installed.packages()[, 1])) {
  install.packages('gmodels', repos = "http://cran.rstudio.com/")
}
if (!is.element('FSelector', installed.packages()[, 1])) {
  install.packages('FSelector', repos = "http://cran.rstudio.com/")
}
if (!is.element('devtools', installed.packages()[, 1])) {
  install.packages('devtools', repos = "http://cran.rstudio.com/")
}
if (!is.element('neuralnet', installed.packages()[, 1])) {
  install.packages('neuralnet', repos = "http://cran.rstudio.com/")
}
if (!is.element('e1071', installed.packages()[, 1])) {
  install.packages('e1071', repos = "http://cran.rstudio.com/")
}
library(devtools)
if (!is.element('ggbiplot', installed.packages()[, 1])) {
  install_github('ggbiplot', 'vqv')
}
