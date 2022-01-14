
# RRF

#'mtry - domyślnie (sqrt(p)), gdzie p - liczba argumentów
#'
newRandomForest <- function(x, y, ntree, mtry){
  return(randomForest(x=x, y=y, ntree=ntree, mtry=mtry))
}

s

# outForest
newOutForest <- function(x, y, ntree, mtry){
  return(outForest(x=x, y=y, num.trees=ntree, num.mtry=mtry))
}

