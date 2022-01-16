
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

getSVMFileName <- function(path, prefixName, idx, g, n, suffixName){
  return(paste(path, osGetPathSlash(), prefixName, "idx_", as.character(idx), 
    "gamma_", as.character(g), "_" , "nu_", as.character(n), ".png", sep = ""))
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
      # nu = 0.1 lub mniejsze powinno działać zawsze
      # im niższe nu, tym większa jest tolerancja na błędy -> gorsze wyniki,
      # jak predykcja całego zbioru jako fałszywego
      tmp <- randomize_kfold(dataset, N)
      randData <- tmp[[1]]
      folds <- tmp[[2]]

      newfoName <- getSVMFolderName(foName, g, n)

      for(idx in N){
        fetchedData <- kfold_cv(folds, randData, idx)
        testD <- fetchedData[[1]]
        trainD <- fetchedData[[2]]

        # create SVM
        svmPedictor <- svmObj(trainD, g, n)
        prediction <- predict(svmPedictor, testD)

        # musi być tutaj, bo inaczej stworzą się puste foldery, gdy SVM nie da sobie rady
        dir.create(file.path(newfoName), recursive = TRUE, showWarnings=FALSE)

        png(getSVMFileName(newfoName, "roc_", idx, g, n, ".png"))
        plot(
          getROC(testD, prediction),
          col = "#619e39",
          lwd = 3,
          main = paste("SVM ", "gamma: ", g, "nu: ", n)
        )
        dev.off()
        
        
      }
    }
  }
  
}