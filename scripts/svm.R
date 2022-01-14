
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


newSVMOne <- function(x, y, gamma, nu){
  return(svm(x=x, y=y, type='one-classification', kernel='radial', gamma=gamma, nu=nu))
}

newSVMTwo <- function(x, y, gamma, nu){
  return(svm(x=x, y=y, type='nu-classification', kernel='radial', gamma=gamma, nu=nu))
}