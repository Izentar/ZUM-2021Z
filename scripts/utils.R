if (!require("pacman"))
  install.packages("pacman")

#'Funkcja ładuje wszsytkie potrzebne biblioteki. Używa do tego menadżera packman.
#'
#'@export
loadPackages <- function() {
  #library(datasets)
  pacman::p_load(pacman, here, psych, dplyr, ggplot2, caret, smotefamily, RRF,  pROC, PRROC, datasets, PRROC, ROSE, e1071, caret)
  source(here::here('scripts', 'fileProcessing.R'))
  source(here::here('scripts', 'runRRF.R'))
  source(here::here('scripts', 'plots.R'))
  source(here::here('scripts', 'svm.R'))
  #source(here::here('scripts', 'outForest.R'))
}


#'Czyści konsolę
#'
#'@export
clear <- function() {
  cat("\014")
}


#' Funkcja czyści całe środowisko, zwalnia biblioteki oraz czyści konsolę.
#'
#'@param clearConsole czy czyścić konsolę
#'
#'@export
terminate <- function(clearConsole = TRUE, restart=FALSE) {
  p_unload(all)
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  if(clearConsole){
    cat("\014")
  }
  if(restart){
    .rs.restartR()
  }
}

divideDataset <- function(dataSet, randSample = TRUE){
  zero <- dataSet[dataSet$Class == "0", ]
  zero <- zero[sample(nrow(zero)), ]
  one <- dataSet[dataSet$Class == "1", ]
  one <- one[sample(nrow(one)), ]

  return(list(zero, one))
}

sampleDataset <- function(dataSet, N){
  tmp <- dataSet[sample(nrow(dataSet)), ]
  return(tmp[1:N, ])
}

concatenateDatasets <- function(d1, d2){
  return(rbind(d1, d2))
}

compareDatasets <- function(d1, d2){
  return(setdiff(d1, d2))
}

# przetestowana
randomize_kfold <- function (dataSet, N) {
  dataTmp <- dataSet[sample(nrow(dataSet)), ]
  
  if(N < 1){
    stop(paste("Bad numer of folds in k-folds:", N))
  }
  list(dataTmp, cut(seq(1, nrow(dataTmp)), breaks = N, labels = FALSE))
}

# przetestowana
kfold_cv <- function(folds, dataSet, i) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- dataSet[testIndexes,]
  trainData <- dataSet[-testIndexes,]
  
  return(list(testData, trainData))
}

plot_AUPRC <- function(testData, predicted, title) {
  fg <- predicted[testData$Class == 1]
  bg <- predicted[testData$Class == 0]
  pr <- pr.curve(scores.class0 = fg,
                 scores.class1 = bg,
                 curve = T)
  return(plot(pr, main = title))
}



getROC <- function(dataUsedInPred, prediction){
  #print(dataUsedInPred)
  #print(prediction)
  #stop("STOP")
  return(roc(prediction, factor(dataUsedInPred$Class, ordered = TRUE)))
}

getFolderPath <- function(folderNames, fileIndex){
  foName <- 'default'
  if(length(folderNames) <= fileIndex){
    return(paste(foName, '_', as.character(fileIndex), sep=""))
  }
  else{
    return(folderNames[[fileIndex]])
  }
}

osGetPathSlash <- function(){
  name <- Sys.info()['sysname']
  if(name == "Linux"){
    return('/')
  }
  else if(name == "Windows"){
    return('\\')
  }
  stop("Unknown operating system detected. Use Linux or Windows.")
}