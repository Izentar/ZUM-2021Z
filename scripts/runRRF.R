
getRRFFolder <- function(path, ntree="default", mtry="default", maxnodes="default"){
  return(paste(path, osGetPathSlash(), "ntree_", ntree, 
               osGetPathSlash(), "mtry_", mtry, "maxnodes_", maxnodes, osGetPathSlash(), sep=""))
}



runRRF <- function(dataSet) {
  N <- 5
  tmp <- randomize_kfold(dataSet, N)
  randData <- tmp[[1]]
  folds <- tmp[[2]]
  foName <- "test" 
  #ntree
  for (i in 1:N) {
    data <- kfold_cv(folds, randData, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    
    for (ntree in seq(10, 300, by = 50)) {
      
      newfoName <- getRRFFolder(foName, ntree = toString(ntree))
      dir.create(file.path(newfoName), recursive = TRUE, showWarnings=FALSE)
      output <- file(description = "test.txt", open = "w") # wa - write append
      outputTerminal <- file(description = "test_terminal.txt", open = "w")
      
      myclassifier_rrf <- RRF(Class ~ ., data = trainData, ntree = ntree)
      prediction_rrf <- predict(myclassifier_rrf, testData[,-31])
      

      ROC <-
        roc(prediction_rrf, factor(testData$Class, ordered = TRUE))
  
      matrix <- confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
      
      
      writeCapturedOutput(matrix, file=outputTerminal)
      
      writeString(output, "confusion matrix:")
      writeString(output, paste("Positive,", toString(mpos)))
      writeCsv(output, mtab)
      writeCsv(output, moverall)
      writeCsv(output, mclass)
      writeString(output, "\n")
      
      
      auc <- auc(ROC)
      
      png(generateFileName(newfoName, "roc", "_ntree_", ntree, ".png"))

      plot(
        ROC,
        col = "red",
        lwd = 3,
        main = paste("RRF ", "ntree: ", ntree, "AUC: ", auc)
      )
      
      dev.off()
      plot_AUPRC(testData, prediction_rrf, paste("RRF ", "ntree: ", ntree))
    }
    
  }

  
  
  #mtry
  for (i in 1:N) {
    data <- kfold_cv(folds, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    for (mtry in seq(2500, 3000, by = 250)) {
      myclassifier_rrf <- RRF(Class ~ ., data = trainData, mtry = mtry)
      prediction_rrf <- predict(myclassifier_rrf, testData[,-31])
      ROC <-
        roc(prediction_rrf, factor(testData$Class, ordered = TRUE))
      confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
      auc <- auc(ROC)
      plot(
        ROC,
        col = "red",
        lwd = 3,
        main = paste("RRF ", "mtry: ", mtry, "AUC: ", auc)
      )
      plot_AUPRC(testData,
                 prediction_rrf,
                 paste("RRF AUPRC", "mtry: ", mtry))
    }
  }
  
  #maxnodes
  for (i in 1:N) {
    data <- kfold_cv(folds, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    
    for (maxnodes in seq(10, 100, by = 10)) {
      myclassifier_rrf <- RRF(Class ~ ., data = trainData, mtry = mtry)
      prediction_rrf <-
        predict(myclassifier_rrf, testData[,-31], maxnodes = maxnodes)
      ROC <-
        roc(prediction_rrf, factor(testData$Class, ordered = TRUE))
      confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
      auc <- auc(ROC)
      plot(
        ROC,
        col = "red",
        lwd = 3,
        main = paste("RRF ", "maxnodes: ", maxnodes, "AUC: ", auc)
      )
      plot_AUPRC(testData,
                 prediction_rrf,
                 paste("RRF AUPRC", "maxnodes: ", maxnodes))
      
    }
  }
  
  
  
  #MIX
  for (i in 1:N) {
    data <- kfold_cv(folds, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    for (ntree in seq(10, 300, by = 50)) {
      for (mtry in seq(3, 9, by = 1)) {
        for (maxnodes in seq(10, 100, by = 10)) {
          myclassifier_rrf <-
            RRF(
              Class ~ .,
              data = trainData,
              ntree = ntree,
              mtry = mtry,
              maxnodes = maxnodes
            )
          prediction_rrf <-
            predict(myclassifier_rrf, testData[,-31])
          ROC <-
            roc(prediction_rrf,
                factor(testData$Class, ordered = TRUE))
          confusionMatrix(as.factor(prediction_rrf),
                          as.factor(testData$Class))
          auc <- auc(ROC)
          print(prediction_rrf)
          plot(
            ROC,
            col = "red",
            lwd = 3,
            main = paste(
              "RRF ",
              "ntree: ",
              ntree,
              "mtry: ",
              mtry,
              "maxnodes: ",
              maxnodes,
              "AUC: ",
              auc
            )
          )
          plot_AUPRC(
            testData,
            prediction_rrf,
            paste(
              "RRF AUPRC ",
              "ntree: ",
              ntree,
              "mtry: ",
              mtry,
              "maxnodes: ",
              maxnodes
            )
          )
        }
      }
    }
  }
  
}