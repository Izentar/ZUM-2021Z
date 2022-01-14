library(datasets)
pacman::p_load(pacman)

prepareData <- function(example = FALSE) {
  dataset <- NULL
  if (example) {
    dataset <- read.table("data/mbb.csv", header = TRUE, sep = ",")
  }
  else{
    dataset <- read.table("data/creditcardshort.txt", header = TRUE, sep = ",")
  }
  #head(dataset)
  
  return(dataset)
}


getSummary <- function(fileName, dataSet, ignore = list()) {
  output <- file(description = fileName, open = "w") # wa - write append
  
  write("Head\n", file = output, append = TRUE)
  write.table(dataSet[1:6,],
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
      d <- as.data.frame(describe(dataSet[[column]]))
      write.csv(d, file = output, row.names = FALSE)
    }
  }
  
  close(output)
}
