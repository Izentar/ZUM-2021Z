#
#
# NOT WORKING
#
#


getoutForestFolder <-
  function(path,
           N,
           max_n_outliers = "default",
           min_node_size = "default") {
    return(
      paste(
        path,
        osGetPathSlash(),
        N,
        "max_n_outliers_",
        max_n_outliers ,
        "_min_node_size_ ",
        min_node_size,
        osGetPathSlash(),
        sep = ""
      )
    )
  }


stratified_sampling <- function(proportion, rows_number, dataset) {
  zero <- dataSet[dataSet$Class == "0", ]
  one <- dataSet[dataSet$Class == "1",]
  
  zeroSampl <- sample(nrow(zero))
  oneSampl <- sample(nrow(one))
  
  one_samples <- sample_n(one, proportion * rows_number, replace = TRUE)
  zero_samples <- sample_n(zero, (1 - proportion) * rows_number, replace = TRUE)

  zeroLow <- zero[zeroSampl]
  zeroHigh <- zero[-zeroSampl]

  oneLow <- one
  
  rbind(zero_samples, one_samples)
}



runOutForest <-
  function(dataSet,
           N = 5,
           extra_validation = NULL,
           max_n_outliers_list =  list(),
           min_node_size_list =list()) {
    #dataSet <- stratified_sampling(0.6, 5000, dataSet)

    foName <- "outForest_2"
    
    for (max_n_outliers in max_n_outliers_list) {
      for (min_node_size in min_node_size_list) {
        for (i in 1:N) {
          #expResult <- tryCatch({  
            
          tmp <- randomize_kfold(dataSet, N)
          randData <- tmp[[1]]
          folds <- tmp[[2]]
          
          newfoName <-
            getoutForestFolder(
              foName,
              i,
              max_n_outliers = toString(max_n_outliers),
              min_node_size = toString(min_node_size)
            )
          dir.create(file.path(newfoName),
                     recursive = TRUE,
                     showWarnings = FALSE)
          
          output <-
            file(description = paste(newfoName, "_notes.txt"),
                 open = "w") # wa - write append
          
          dir.create(file.path(newfoName),
                     recursive = TRUE,
                     showWarnings = FALSE)
          
          data <- kfold_cv(folds, randData, i)
          testData <- data[[1]]
          testData$Class <- as.factor(testData$Class)
          
          if (!is.null(extra_validation)) {
            testData <- concatenateDatasets(testData, extra_validation)
          }
          
          trainData <- data[[2]]
          trainData$Class <- as.factor(trainData$Class)
          trainData <- trainData[, -20]
          classifier <-
            outForest(trainData,
                      max_n_outliers = max_n_outliers,
                      allow_predictions = TRUE)
          
          prediction <- predict(classifier, newdata  = testData[, -20])
          outli <- outliers(prediction)
          predictedData <- testData
          
          predictedData$Class <- 0
          predictedData$Class[c(outli$row)] <- 1
          
          ROC <-
            roc(predictedData$Class,
                factor(testData$Class, ordered = TRUE))
          
          matrix <-
            confusionMatrix(as.factor(predictedData$Class),
                            as.factor(testData$Class))
          writeCapturedOutput(matrix, file = paste(newfoName, "confusionMatrix.txt"))
          
          
          svmPrecision <-
            precision(as.factor(predictedData$Class),
                      as.factor(testData$Class))
          
          writeString(output, "precision:")
          writeString(output, toString(svmPrecision))
          writeString(output, "\n")
          
          svmRecall <-
            recall(as.factor(predictedData$Class),
                   as.factor(testData$Class))
          writeString(output, "recall:")
          writeString(output, toString(svmRecall))
          writeString(output, "\n")
          
          f1_score <-
            (svmPrecision * svmRecall) / (svmPrecision + svmRecall)
          writeString(output, "f1 score:")
          writeString(output, toString(f1_score))
          writeString(output, "\n")
          
          auc <- auc(ROC)
          writeString(output, "AUC:")
          writeString(output, toString(auc))
          writeString(output, "\n")
          
          
          png(paste(newfoName, "roc", ".png"))
          plot(
            ROC,
            col = "red",
            lwd = 3,
            main =  paste(
              "outForest ",
              "max_n_outliers: ",
              max_n_outliers,
              "min_node_size: ",
              min_node_size
            )
          )
          
          dev.off()
          
          png(paste(newfoName, "auprc", ".png"))
          plot_AUPRC(
            testData,
            predictedData$Class,
            paste(
              "outForest ",
              "max_n_outliers: ",
              max_n_outliers,
              "min_node_size: ",
              min_node_size
            )
          )
          
          dev.off()
          
          close(output)
          
        #},
        #error = function(cond) {
        #  print(
        #    paste(
        #      "ERROR: at max_n_outliers",
        #      toString(max_n_outliers),
        #      "min_node_size_list",
        #      toString(min_node_size_list),
        #      toString(Sys.time()),
        #      "Message:",
        #      toString(cond)
        #    )
        #  )
        #},
        #warning = function(cond) {
        #  print(
        #    paste(
        #      "WARNING:  at max_n_outliers",
        #      toString(max_n_outliers),
        #      "min_node_size_list",
        #      toString(min_node_size_list),
        #      "time",
        #      toString(Sys.time()),
        #      "Message:",
        #      toString(cond)
        #    )
        #  )
        #},
        #finally = {
        #  print(
        #    paste(
        #      "END: at max_n_outliers",
        #      toString(max_n_outliers),
        #      "min_node_size_list",
        #      toString(min_node_size_list),
        #      "time",
        #      toString(Sys.time())))
        #  
          
        #})
      }
      
      }
    }
  }