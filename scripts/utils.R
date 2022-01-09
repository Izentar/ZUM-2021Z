install.packages("caret")
install.packages("smotefamily")
install.packages("RRF")
install.packages("pROC")
install.packages("ROSE")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("PRROC")

library(dplyr)
library(ggplot2)
library(caret)
library(smotefamily)
library(RRF)
library(pROC)
library(datasets)
library(PRROC)
if (!require("pacman")) install.packages("pacman")

#'Funkcja ładuje wszsytkie potrzebne biblioteki. Używa do tego menadżera packman.
#'
#'@export
loadPackages <- function(){
  #library(datasets)
  pacman::p_load(pacman, here, psych) 
  source(here::here('scripts', 'fileProcessing.R'))
  source(here::here('scripts', 'runRRF.R'))
  source(here::here('scripts', 'plots.R'))
}


#'Czyści konsolę
#'
#'@export
clear <- function(){
  cat("\014")
}


#' Funkcja czyści całe środowisko, zwalnia biblioteki oraz czyści konsolę. 
#'
#'@param clearConsole czy czyścić konsolę
#'
#'@export
terminate<- function(clearConsole=TRUE){
  p_unload(all)
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv ) 
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  cat("\014")
}

kfold_cv <- function(dataSet, i, n ) {
  dataSet<-dataSet[sample(nrow(dataSet)),]
  
  folds <- cut(seq(1,nrow(dataSet)),breaks=n,labels=FALSE)
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dataSet[testIndexes, ]
  trainData <- dataSet[-testIndexes, ]
  
  list(testData, trainData)
}

plot_AUPRC <- function(testData, predicted, title) {
  fg <- predicted[testData$Class==1]
  bg <- predicted[testData$Class==0]
  pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  plot(pr, main=title)
}