
stratified_sampling <- function(proportion, rows_number, dataset) {
  zero <- dataSet[dataSet$Class == "0",]
  one <- dataSet[dataSet$Class == "1", ]
  
  
  one_samples <- sample_n(one, proportion * rows_number)
  zero_samples <- sample_n(zero, (1-proportion) * rows_number)
  
  zero_samples
}


runOutForest <- function(dataSet) {
  
  dataSet <- stratified_sampling(0.7, 200, dataSet)
  N = 5
  tmp <- randomize_kfold(dataSet,N)
  randData <- tmp[[1]]
  folds <- tmp[[2]]
  
  #print(dataSet[dataSet$Class == "0", ])

  for (i in 1:N) {
    data <- kfold_cv(folds, randData, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    
    classifier <- outForest(trainData, allow_predictions=TRUE)
    testData$V10 <- 100.0
    
    prediction <- predict(classifier, newdata  = testData)
    print("")
    outli <- outliers(prediction)
    selectedByAlg <- testData[outli$row, ]
    print(outli$row)
    print(selectedByAlg)
    onlyOne <- selectedByAlg[selectedByAlg$Class == "1", ]
    print(nrow(onlyOne))
    print(nrow(selectedByAlg))
    stop("STOP")
    print(outliers(prediction)$row)
    
  }
  
}