library(datasets)
if (!require("pacman")) install.packages("pacman")

?library

# wszystkie ogólnego przeznaczeniea paczki tutaj
pacman::p_load(pacman, here) 

loadPackages <- function(){
  library(datasets)
  pacman::p_load(pacman, here) 
  source(here::here('scripts', 'fileProcessing.R'))
}


terminate<- function(){
  p_unload(all)
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv ) 
  cat("\014")
}