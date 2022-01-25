

getRRFFolder <-
  function(path,
           N,
           ntree = "default",
           mtry = "default",
           maxnodes = "default") {
    return(
      paste(
        path,
        osGetPathSlash(),
        "N_",
        N,
        "_ntree_ ",
        ntree,
        "_mtry_",
        mtry,
        "_maxnodes_",
        maxnodes,
        osGetPathSlash(),
        sep = ""
      )
    )
  }


runRRF <- function(dataSet,
                   extra_validation = NULL,
                   N = 5,
                   ntree_list = list(),
                   mtry_list = list(),
                   maxnodes_list = list()) {
  foName <- "RRF"
  for (ntree in ntree_list) {
    for (mtry in mtry_list) {
      for (maxnodes in maxnodes_list) {
        tmp <- randomize_kfold(dataSet, N)
        randData <- tmp[[1]]
        folds <- tmp[[2]]
        
        
        #MIX
        for (i in 1:N) {
          expResult <- tryCatch({
            data <- kfold_cv(folds, randData, i)
            testData <- data[[1]]
            
            if(! is.null(extra_validation)){
              testData <- concatenateDatasets(testData, extra_validation)
            }
            
            testData$Class <- as.factor(testData$Class)
            
            trainData <- data[[2]]
            trainData$Class <- as.factor(trainData$Class)
            

            print(paste("START: at ntree", ntree, "mtry", mtry, "maxnodes", maxnodes,"time",toString(Sys.time())))
            
            newfoName <-
              getRRFFolder(foName,
                           i,
                           maxnodes = toString(maxnodes),
                           mtry = toString(mtry))
            dir.create(file.path(newfoName),
                       recursive = TRUE,
                       showWarnings = FALSE)
            
            myclassifier_rrf <-
              RRF(
                Class ~ .,
                data = trainData,
                ntree = ntree,
                mtry = mtry,
                maxnodes = maxnodes
              )
            prediction_rrf <-
              predict(myclassifier_rrf, testData[, -20])
            ROC <-
              roc(prediction_rrf,
                  factor(testData$Class, ordered = TRUE))
            confusionMatrix(as.factor(prediction_rrf),
                            as.factor(testData$Class))
            matrix <-
              confusionMatrix(as.factor(prediction_rrf),
                              as.factor(testData$Class))
            writeCapturedOutput(matrix, file = paste(newfoName, "confusionMatrix.txt"))
            
            auc <- auc(ROC)
            png(
              paste(
                newfoName,
                "roc",
                "_maxnodes_",
                maxnodes,
                "_mtry_",
                mtry,
                "_ntree_" ,
                ntree,
                ".png"
              )
            )
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
            
            png(
              paste(
                newfoName,
                "roc",
                "_maxnodes_",
                maxnodes,
                "_mtry_",
                mtry,
                "_ntree_" ,
                ntree,
                ".png"
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
            dev.off()
          },
          error = function(cond) {
            print(
              paste(
                "ERROR: at ntree",
                toString(ntree),
                "mtry",
                toString(mtry),
                "maxnodes",
                toString(maxnodes),
                "time",
                toString(Sys.time()),
                "Message:",
                toString(cond)
              )
            )
          },
          warning = function(cond) {
            print(
              paste(
                "WARNING:  at ntree",
                toString(ntree),
                "mtry",
                toString(mtry),
                "maxnodes",
                toString(maxnodes),
                "time",
                toString(Sys.time()),
                "Message:",
                toString(cond)
              )
            )
          },
          finally = {
            print(
              paste(
                "END: at at ntree",
                toString(ntree),
                "mtry",
                toString(mtry),
                "maxnodes",
                toString(maxnodes),
                "time",
                toString(Sys.time())))
              
            
          })
        }
      }
    }
  }
  
}