if (!require("pacman"))
  install.packages("pacman")

#'Funkcja ładuje wszsytkie potrzebne biblioteki. Używa do tego menadżera packman.
#'
#'@export
loadPackages <- function() {
  #library(datasets)
  pacman::p_load(pacman, here, psych, dplyr, ggplot2, caret, smotefamily, RRF,  pROC, datasets, PRROC, ROSE)
  source(here::here('scripts', 'fileProcessing.R'))
  source(here::here('scripts', 'runRRF.R'))
  source(here::here('scripts', 'plots.R'))
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
  
  rm(list = ls(name = env), pos = env)
  cat("\014")
}

randomize_kfold <- function (dataSet, N) {
  dataSet <- dataSet[sample(nrow(dataSet)), ]
  
  cut(seq(1, nrow(dataSet)), breaks = N, labels = FALSE)
}

kfold_cv <- function(folds, i) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- dataSet[testIndexes,]
  trainData <- dataSet[-testIndexes,]
  
  list(testData, trainData)
}

plot_AUPRC <- function(testData, predicted, title) {
  fg <- predicted[testData$Class == 1]
  bg <- predicted[testData$Class == 0]
  pr <- pr.curve(scores.class0 = fg,
                 scores.class1 = bg,
                 curve = T)
  plot(pr, main = title)
}