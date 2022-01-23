
stratified_sampling <- function(proportion, rows_number, dataset) {
  zero <- dataSet[dataSet$Class == "0",]
  one <- dataSet[dataSet$Class == "1", ]
  
  
  one_samples <- sample_n(one, proportion * rows_number)
  zero_samples <- sample_n(zero, (1-proportion) * rows_number)
  
  rbind(zero_samples, one_samples)
}


runOutForest <- function(dataSet) {
  
  dataSet <- stratified_sampling(0.6, 200, dataSet)
  N = 5
  tmp <- randomize_kfold(dataSet,N)
  randData <- tmp[[1]]
  folds <- tmp[[2]]
  
  #print(dataSet[dataSet$Class == "0", ])

  for (i in 1:N) {
    for (max_n_outliers in seq(2, 200, by = 50)) {
      for (min_node_size in seq (2, 200, by= 40)) {
    data <- kfold_cv(folds, randData, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    classifier <- outForest(trainData,max_n_outliers =max_n_outliers, allow_predictions=TRUE)

    prediction <- predict(classifier, newdata  = testData)
    outli <- outliers(prediction)
    tmptestData <- testData
    #tmptestData$Class <- as.integer(tmptestData$Class)

    tmptestData$Class[c(outli$row) ] <- 1

    #stop("S")
    ROC <- roc(tmptestData$Class, factor(testData$Class, ordered = TRUE))
    auc <- auc(ROC)
    
    plot(
      ROC,
      col = "red",
      lwd = 3,
      main =  paste("outForest ", "max_n_outliers: ", max_n_outliers,"min_node_size: ", min_node_size  )
    )
    print("testData")
    print(testData$Class)
    
    print("tmptestData")
    print(tmptestData$Class)
    plot_AUPRC(testData,
               tmptestData$Class,
               paste("outForest ", "max_n_outliers: ", max_n_outliers,"min_node_size: ", min_node_size  ))

    }
  }
  
  }
}