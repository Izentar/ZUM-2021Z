library(datasets)
pacman::p_load(pacman)

prepareData <- function(){
  dataset <- read.table("data/creditcard.csv", header=TRUE, sep=",") 
  #dataset <- read.table("data/mbb.csv", header=header, sep=",") 
  #head(dataset)

  return(dataset)
}


getSummary <- function(fileName, dataSet){
  output <- file(fileName)
  write.table(dataSet[1:6, ], file=output, sep=",")
  writeLines("\n\n", output)
}
