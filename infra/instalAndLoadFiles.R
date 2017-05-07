loadAndInstallPackages <- function() {
  packagesToInsall <- read.table(file = "packages.txt", header = FALSE)[[1]]
  for (currentPackage in packagesToInsall) {
    if (!is.element(currentPackage, installed.packages()[, 1])) {
      loginfo(sprintf("Intalacja pakietu %s", currentPackage))
      install.packages(currentPackage, repos = "http://cran.rstudio.com/")
    } else{
      loginfo(sprintf("Pakiet %s jest juz zainstalowany", currentPackage))
    }
    library(currentPackage, character.only = T)
    loginfo(sprintf("Pakiet %s zostal pomyslnie zaladowany", currentPackage))
  }
}

sourceRFiles <- function() {
  files <- retrieveAllRFilesPaths()
  for (currentFile in files) {
    loginfo(sprintf("Wczytywanie pliku %s", currentFile))
    source(currentFile)
  }
}

retrieveAllRFilesPaths <- function() {
  baseRDirPath <- getCustomProperty("baseRDirPath")
  allRFilePaths <- list.files(baseRDirPath, pattern = ".*.[rR]", recursive = TRUE)
  return(paste(baseRDirPath, allRFilePaths, sep = "/"))
}

retrieveAllTestFilesPaths <- function() {
  baseTestDirectory <- getCustomProperty("baseTestDirectory")
  allRFilePaths <- list.files(baseTestDirectory, pattern = "*.[Rr]$")
  return(allRFilePaths)
}

startLoggerToFile <- function(){
  if(getCustomProperty("isLoggingToFileOn" && !isCurrentPerformByJenkins())){
    logFile<-file(paste(getCustomProperty("logPath"), format(Sys.time(), format="%H.%M_%m-%d-%Y"), getCustomProperty("logFile"),sep=""), blocking = FALSE, open="wt")
    sink(logFile ,  append = TRUE, type =  "message", split = FALSE)
  }
}

getCustomProperty <- function(propertyName) {
  stopifnot(is.character((propertyName)))
  property <- read.properties("properties.txt")[[propertyName]]
  return (property)
}

convertToAbsolutePath <- function(path) {
  return (paste(getwd(), path, sep = "/"))
}

isCurrentPerformByJenkins <- function(){
  return(grepl("19614774", system("hostname", intern = TRUE)))
}

setupEnvironment <- function(){
  if (!is.element('logging', installed.packages()[, 1])) {
    install.packages('logging', repos = "http://cran.rstudio.com/")
  }
  if (!is.element('properties', installed.packages()[, 1])) {
    install.packages('properties', repos = "http://cran.rstudio.com/")
  }
  library(logging)
  library(properties)
  basicConfig() # logger
  loadAndInstallPackages()
  sourceRFiles()
  
  # ########################################### #
  if(getCustomProperty("loggingToFile")){
    if(!file.exists("logs")){
      dir.create("logs")
    }
    logFilePath <-paste0('logs/session.log')
    addHandler(writeToFile, file=logFilePath)
  }
}
