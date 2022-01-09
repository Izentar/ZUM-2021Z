library(datasets)
if (!require("pacman")) install.packages("pacman")

#'Funkcja ładuje wszsytkie potrzebne biblioteki. Używa do tego menadżera packman.
#'
#'@export
loadPackages <- function(){
  #library(datasets)
  pacman::p_load(pacman, here, psych, outForest, RRF, e1071) 
  source(here::here('scripts', 'fileProcessing.R'))
  source(here::here('scripts', 'svm.R'))
  source(here::here('scripts', 'forest.R'))
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
terminate<- function(clearConsole=TRUE, restart=FALSE){
  p_unload(all)
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv ) 
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
  if(clearConsole){
    cat("\014")
  }
  if(restart){
    .rs.restartR()
  }
  
}