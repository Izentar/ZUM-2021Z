library(datasets)
pacman::p_load(pacman)

prepareData <- function(example = FALSE, tiny = FALSE) {
  dataset <- NULL
  if (example) {
    if(tiny){
      dataset <- read.table("data/creditcardtiny.csv", header = TRUE, sep = ",")
    }
    else{
      dataset <- read.table("data/creditcardtest.csv", header = TRUE, sep = ",")
    }
  }
  else{
    dataset <- read.table("data/creditcard15000.csv", header = TRUE, sep = ",")
  }
  #head(dataset)
  
  return(dataset)
}

writeCapturedOutput <-function(fileHandler, obj){
  capture.output(obj, file = fileHandler, append = TRUE)
}

writeString <- function(fileHandler, string){
  write(string, file = fileHandler, append = TRUE)
}

writeTable <- function(fileHandler, dataSet, ignore = list(), toInteger = list('Class')){
  write.table(data.frame(dataSet[seq_len(dataSet), ]),
    file = fileHandler,
    sep = ",",
    row.names = FALSE)
}

writeCsv <- function(fileHandler, dataSet, ignore = list(), toInteger = list('Class')){
  write.csv(data.frame(dataSet),
    file = fileHandler,
    row.names = FALSE)
}

# przykładowa funkcja, która pokazuje jak się zapisuje rzeczy do pliku
getSummary <- function(fileName, dataSet, ignore = list(), toInteger = list('Class')) {
  output <- file(description = fileName, open = "w") # wa - write append
  
  write("Head\n", file = output, append = TRUE)
  write.table(dataSet[seq_len(dataSet), ],
              file = output,
              sep = ",",
              row.names = FALSE)
  
  write("\n\nSummary\n", file = output, append = TRUE)
  s <- summary(dataSet)
  write.table(s,
              file = output,
              sep = ",",
              row.names = FALSE)
  
  write("\n\nDescribe\n", file = output, append = TRUE)
  for (column in names(dataSet)) {
    write(
      paste(
        "\n\nDescribe_column ",
        column,
        sep = "",
        collapse = NULL
      ),
      file = output,
      append = TRUE
    )
    if (!(column %in% ignore)) {
      c <- dataSet[[column]]
      if(column %in% toInteger){
        c <- as.integer(c)
      }
      d <- as.data.frame(describe(c))
      write.csv(d, file = output, row.names = FALSE)
    }
  }
  
  close(output)
}
