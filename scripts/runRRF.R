runRRF <- function(dataSet) {

N = 5

#ntree 
for (i in 1:N) {
  data <- kfold_cv(dataSet, i, N)
  testData <- data[[1]]
  testData$Class <- as.factor(testData$Class)
  trainData <- data[[2]]
  trainData$Class <- as.factor(trainData$Class)
  
  for (ntree in seq(10, 300, by = 50)) {
    myclassifier_rrf <- RRF(Class ~ ., data=trainData, ntree=ntree)
    prediction_rrf <- predict(myclassifier_rrf, testData[, -31] )
    ROC <- roc( prediction_rrf, factor(testData$Class, ordered = TRUE))
    confusionMatrix(as.factor(prediction_rrf), as.factor(testData$Class))
    auc<-auc(ROC)
    plot(ROC, col="red", lwd=3, main=paste("RRF ", "ntree: ", ntree, "AUC: ", auc))
    plot_AUPRC(testData,prediction_rrf, paste("RRF ", "ntree: ", ntree))
  }
}

#mtry 
for (i in 1:N) {
  data <- kfold_cv(dataSet, i, N)
  testData <- data[[1]]
  testData$Class <- as.factor(testData$Class)
  trainData <- data[[2]]
  trainData$Class <- as.factor(trainData$Class)
  for (mtry in seq(2500, 3000, by = 250)) {
    myclassifier_rrf <- RRF(Class ~ ., data=trainData, mtry=mtry)
    prediction_rrf <- predict(myclassifier_rrf, testData[, -31] )
    ROC <- roc( prediction_rrf, factor(testData$Class, ordered = TRUE))
    confusionMatrix(as.factor(prediction_rrf),as.factor(testData$Class) )
    auc<-auc(ROC)
    plot(ROC, col="red", lwd=3, main=paste("RRF ", "mtry: ", mtry, "AUC: ", auc))
    plot_AUPRC(testData, prediction_rrf, paste("RRF AUPRC", "mtry: ", mtry ) )
  }
}

#maxnodes 
for (i in 1:N) {
  data <- kfold_cv(dataSet, i, N)
  testData <- data[[1]]
  testData$Class <- as.factor(testData$Class)
  trainData <- data[[2]]
  trainData$Class <- as.factor(trainData$Class)
  
  for (maxnodes in seq(10, 100, by = 10)) {
    myclassifier_rrf <- RRF(Class ~ ., data=trainData, mtry=mtry)
    prediction_rrf <- predict(myclassifier_rrf, testData[, -31], maxnodes=maxnodes )
    ROC <- roc( prediction_rrf, factor(testData$Class, ordered = TRUE))
    confusionMatrix(as.factor(prediction_rrf),as.factor(testData$Class) )
    auc<-auc(ROC)
    plot(ROC, col="red", lwd=3, main=paste("RRF ", "maxnodes: ", maxnodes, "AUC: ", auc))
    plot_AUPRC(testData, prediction_rrf, paste("RRF AUPRC", "maxnodes: ", maxnodes))
    
  }
}



#MIX
for (i in 1:N) {
  data <- kfold_cv(dataSet, i, N)
  testData <- data[[1]]
  testData$Class <- as.factor(testData$Class)
  trainData <- data[[2]]
  trainData$Class <- as.factor(trainData$Class)
  for (ntree in seq(10, 300, by = 50)) {
    for (mtry in seq(3, 9, by = 1 )) {
      for (maxnodes in seq(10, 100, by = 10)) {
        myclassifier_rrf <- RRF(Class ~ ., data=trainData, ntree=ntree, mtry=mtry, maxnodes=maxnodes)
        prediction_rrf <- predict(myclassifier_rrf, testData[, -31] )
        ROC <- roc( prediction_rrf, factor(testData$Class, ordered = TRUE))
        confusionMatrix(as.factor(prediction_rrf),as.factor(testData$Class) )
        auc<-auc(ROC)
        plot(ROC, col="red", lwd=3, main=paste("RRF ", "ntree: ", ntree, "mtry: ", mtry,  "maxnodes: ", maxnodes, "AUC: ", auc))
        plot_AUPRC(testData, prediction_rrf, paste("RRF AUPRC ", "ntree: ", ntree, "mtry: ", mtry,  "maxnodes: ", maxnodes))
        }
    }
  }
}

}