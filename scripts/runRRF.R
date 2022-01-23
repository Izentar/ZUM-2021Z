
getRRFFolder <- function(path, N, ntree="default", mtry="default", maxnodes="default"){
  return(paste(path, osGetPathSlash(), "N_", N, "_ntree_ ", ntree, "_mtry_", mtry, "_maxnodes_", maxnodes, osGetPathSlash(), sep=""))
}


runRRF <- function(dataSet, extra_validation=NULL) {
  N <- 5
  tmp <- randomize_kfold(dataSet, N)
  randData <- tmp[[1]]
  folds <- tmp[[2]]
  foName <- "RRF" 
  
  #ntree
  for (i in 1:N) {
    data <- kfold_cv(folds, randData, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)

    if(! is.null(extra_validation)){
      testData <- concatenateDatasets(testData, extra_validation)
    }
    
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    
    for (ntree in seq(10, 300, by = 50)) {
      
      newfoName <- getRRFFolder(foName, N= i, ntree = toString(ntree))
      dir.create(file.path(newfoName), recursive = TRUE, showWarnings=FALSE)
      output <- file(description = paste(newfoName, "_notes.txt"), open = "w") # wa - write append
      
      
      
      myclassifier_rrf <- RRF(Class ~ ., data = trainData, ntree = ntree)
      prediction_rrf <- predict(myclassifier_rrf, testData[,-31])
      

      ROC <-
        roc(prediction_rrf, factor(testData$Class, ordered = TRUE))
  
      matrix <- confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
      writeCapturedOutput(matrix, file=paste(newfoName, "confusionMatrix.txt"))
      
      
      
      auc <- auc(ROC)
      
      svmPrecision <- precision(as.factor(prediction_rrf), as.factor(testData$Class))
      
      writeString(output, "precision:")
      writeString(output, toString(svmPrecision))
      writeString(output, "\n")
      
      svmRecall <- recall(as.factor(prediction_rrf), as.factor(testData$Class))
      writeString(output, "recall:")
      writeString(output, toString(svmRecall))
      writeString(output, "\n")
      
      f1_score <- (svmPrecision * svmRecall) / (svmPrecision + svmRecall)
      writeString(output, "f1 score:")
      writeString(output, toString(f1_score))
      writeString(output, "\n")
      
      auc <- auc(ROC)
      writeString(output, "AUC:")
      writeString(output, toString(auc))
      writeString(output, "\n")
      
      close(output)
      
      png(paste(newfoName, "roc", "_ntree_", ntree, ".png"))
      plot(
        ROC,
        col = "red",
        lwd = 3,
        main = paste("RRF ", "ntree: ", ntree, "AUC: ", auc)
      )
      dev.off()
      
      png(paste(newfoName, "roc", "_ntree_", ntree, "_AUPRC_" , ".png"))
      plot_AUPRC(testData, prediction_rrf, paste("RRF ", "ntree: ", ntree))
      dev.off()
    }
    
  }

  
  
  #mtry
  for (i in 1:N) {
    
    data <- kfold_cv(folds, randData, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    for (mtry in seq(2500, 3000, by = 250)) {
      newfoName <- getRRFFolder(foName,i, mtry = toString(mtry))
      dir.create(file.path(newfoName), recursive = TRUE, showWarnings=FALSE)
      
      
      myclassifier_rrf <- RRF(Class ~ ., data = trainData, mtry = mtry)
      prediction_rrf <- predict(myclassifier_rrf, testData[,-31])
      
      matrix <- confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
      writeCapturedOutput(matrix, file=paste(newfoName, "confusionMatrix.txt"))
      ROC <-
        roc(prediction_rrf, factor(testData$Class, ordered = TRUE))
      
      
      confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
      auc <- auc(ROC)
      
      png(paste(newfoName, "roc", "_mtry_", mtry, ".png"))
      plot(
        ROC,
        col = "red",
        lwd = 3,
        main = paste("RRF ", "mtry: ", mtry, "AUC: ", auc)
      )
      dev.off()
      
      png(paste(newfoName, "roc", "_mtry_", mtry, "_AUPRC_" , ".png"))
      plot_AUPRC(testData,
                 prediction_rrf,
                 paste("RRF AUPRC", "mtry: ", mtry))
      dev.off()
    }
  }
  
  #maxnodes
  for (i in 1:N) {
    
    data <- kfold_cv(folds, randData, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    
    for (maxnodes in seq(10, 100, by = 10)) {
      newfoName <- getRRFFolder(foName,i, maxnodes = toString(maxnodes))
      dir.create(file.path(newfoName), recursive = TRUE, showWarnings=FALSE)
      
      myclassifier_rrf <- RRF(Class ~ ., data = trainData, mtry = mtry)
      prediction_rrf <-
        predict(myclassifier_rrf, testData[,-31], maxnodes = maxnodes)
      
      matrix <- confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
      writeCapturedOutput(matrix, file=paste(newfoName, "confusionMatrix.txt"))
      
      ROC <-
        roc(prediction_rrf, factor(testData$Class, ordered = TRUE))
      
      
      png(paste(newfoName, "roc", "_maxnodes_", maxnodes, "_AUPRC_" , ".png"))
      auc <- auc(ROC)
      plot(
        ROC,
        col = "red",
        lwd = 3,
        main = paste("RRF ", "maxnodes: ", maxnodes, "AUC: ", auc)
      )
      
      dev.off()
      
      png(paste(newfoName, "roc", "_maxnodes_", maxnodes, "_AUPRC_" , ".png"))
      plot_AUPRC(testData,
                 prediction_rrf,
                 paste("RRF AUPRC", "maxnodes: ", maxnodes))
      dev.off()
      
    }
  }
  
  
  
  #MIX
  for (i in 1:N) {
    data <- kfold_cv(folds, randData, i)
    testData <- data[[1]]
    testData$Class <- as.factor(testData$Class)
    trainData <- data[[2]]
    trainData$Class <- as.factor(trainData$Class)
    for (ntree in seq(10, 150, by = 50)) {
      for (mtry in seq(3, 9, by = 1)) {
        for (maxnodes in seq(10, 100, by = 10)) {
          newfoName <- getRRFFolder(foName,i, maxnodes = toString(maxnodes), mtry = toString(mtry))
          dir.create(file.path(newfoName), recursive = TRUE, showWarnings=FALSE)
          
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
          matrix <- confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
          writeCapturedOutput(matrix, file=paste(newfoName, "confusionMatrix.txt"))
          
          auc <- auc(ROC)
          png(paste(newfoName, "roc", "_maxnodes_", maxnodes, "_mtry_", mtry, "_ntree_" , ntree, ".png"))
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
          dev.off()
          
          png(paste(newfoName, "roc", "_maxnodes_", maxnodes, "_mtry_", mtry, "_ntree_" , ntree, ".png"))
          
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
          dev.off()
        }
      }
    }
  }
  
}