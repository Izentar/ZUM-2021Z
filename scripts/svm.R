
# e1071

#'
#'x - wejście - dane w postaci macierzy
#'y - wyjście - wektor zawierający etykiety, które model potrafi przewidzieć
#'
#'
#'type:
#'   * nu-classification
#'   * one-classification
#'   
#'nu - [0, 1] https://stats.stackexchange.com/questions/312897/c-classification-svm-vs-nu-classification-svm-in-e1071-r
#'gamma - [0, 1]
#'


newSVMOneC <- function(data, gamma, nu){
  return(svm(Class ~ ., data = data, type='one-classification', kernel='radial', gamma=gamma, nu=nu))
}

newSVMTwoC <- function(data, gamma, nu){
  return(svm(Class ~ ., data = data, type='nu-classification', kernel='radial', gamma=gamma, nu=nu))
}

generateFileName <- function(path, prefixName, idx = NULL, g, n, suffixName){
  if(is.null(idx)){
    return(paste(path, osGetPathSlash(), prefixName, '_',
    "gamma_", as.character(g), "_" , "nu_", as.character(n), suffixName, sep = ""))
  }
  return(paste(path, osGetPathSlash(), prefixName, '_', "idx_", as.character(idx), "_",
    "gamma_", as.character(g), "_" , "nu_", as.character(n), suffixName, sep = ""))
}

getSVMFolderName <- function(path, g, n){
  return(paste(path, osGetPathSlash(), "gamma_", as.character(g), 
    osGetPathSlash(), "nu_", as.character(n), osGetPathSlash(), sep=""))
}

# uwaga, przed podaniem dataset należy go obrobić zgodnie z przekazanym typem w svmObj
# jeżeli jest Two, to można pozostawić bez zmian
# jeżeli jest One, to trzeba wybrać względem której klasy chcemy nauczyć SVM
experimentSVM <- function(dataset, svmObj, folderName, N=5, gamma = list(), nu = list()){
  osSep <- osGetPathSlash()

  foName <- folderName
  dir.create(file.path(foName), recursive = TRUE, showWarnings=FALSE)
  
  for (g in gamma){
    for (n in nu){ # https://stackoverflow.com/questions/26987248/nu-is-infeasible

      expResult <- tryCatch(
        {
          print(paste("START: at gamma", g, "nu", n, "time", toString(Sys.time())))
          # nu = 0.1 lub mniejsze powinno działać zawsze
          # im niższe nu, tym większa jest tolerancja na błędy -> gorsze wyniki,
          # jak predykcja całego zbioru jako fałszywego
          tmp <- randomize_kfold(dataset, N)
          randData <- tmp[[1]]
          folds <- tmp[[2]]

          newfoName <- getSVMFolderName(foName, g, n)
          dir.create(file.path(newfoName), recursive = TRUE, showWarnings=FALSE)
          output <- file(description = generateFileName(newfoName, "result", idx = NULL, g, n, ".txt"), open = "w") # wa - write append
          outputTerminal <- file(description = generateFileName(newfoName, "resultTerminal", idx = NULL, g, n, ".txt"), open = "w")

          for(idx in 1:N){
            fetchedData <- kfold_cv(folds, randData, idx)
            testD <- fetchedData[[1]]
            trainD <- fetchedData[[2]]

            # create SVM
            svmPedictor <- svmObj(trainD, g, n)
            prediction <- predict(svmPedictor, testD)
            rocObj <- getROC(testD, prediction)

            png(generateFileName(newfoName, "roc", idx, g, n, ".png"))
            plot(
              rocObj,
              col = "#619e39",
              lwd = 3,
              main = paste("SVM ", "gamma: ", g, "nu: ", n)
            )
            dev.off()

            png(generateFileName(newfoName, "AUC_PR", idx, g, n, ".png"))
            plot(
              rocObj,
              col = "#619e39",
              lwd = 3,
              main = paste("AUC PR ", "gamma: ", g, "nu: ", n)
            )
            dev.off()

            matrix <- (confusionMatrix(as.factor(prediction), as.factor(testD$Class)))
            mpos <- matrix$positive
            mtab <- t(matrix$table)
            moverall <- t(matrix$overall)
            mclass <- t(matrix$byClass)

            writeCapturedOutput(matrix, file=outputTerminal)

            writeString(output, "confusion matrix:")
            writeString(output, paste("Positive,", toString(mpos)))
            writeCsv(output, mtab)
            writeCsv(output, moverall)
            writeCsv(output, mclass)
            writeString(output, "\n")

            svmPrecision <- precision(as.factor(prediction), as.factor(testD$Class))

            writeString(output, "precision:")
            writeString(output, toString(svmPrecision))
            writeString(output, "\n")

            svmRecall <- recall(as.factor(prediction), as.factor(testD$Class))
            writeString(output, "recall:")
            writeString(output, toString(svmRecall))
            writeString(output, "\n")

            f1_score <- (svmPrecision * svmRecall) / (svmPrecision + svmRecall)
            writeString(output, "f1 score:")
            writeString(output, toString(f1_score))
            writeString(output, "\n")

            aucScore <- auc(rocObj)
            writeString(output, "AUC:")
            writeString(output, toString(aucScore))
            writeString(output, "\n")

          }
        
            close(output)
            close(outputTerminal)
        },
        error=function(cond){
          print(paste("ERROR: at gamma", toString(g), "nu", toString(n), "time", toString(Sys.time())))
        },
        warning=function(cond){
          print(paste("WARNING: at gamma", toString(g), "nu", toString(n), "time", toString(Sys.time())))
        },
        finally={
          print(paste("END: at gamma", g, "nu", n, "time", toString(Sys.time())))
        })
    }
  }
  
}