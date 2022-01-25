
getImportance <- function(dataSet){
    control <- caret::trainControl(method="repeatedcv", number=5, repeats=1)
    model <- caret::train(Class~., data=dataSet, method="rf", trControl=control, importance = TRUE)
    
    model <- randomForest(factor(Class)~., data=dataSet,  importance = TRUE)

    importance(model)
    varImpPlot(model)
}


sink(file = "outputnew.log.txt", append=TRUE)
#sink(file = NULL)

source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)


dataSet <- prepareData(FALSE, FALSE)
run_plots(dataSet, "_unbalanced")

dataSet = subset(dataSet, select = -c(V15,V7, V18, V8, V1, V5, V2, V28, V25, V23, V27))

run_plots(dataSet, "_unbalanced_2")

dataSet <-  SMOTE(dataSet[,-20], dataSet$Class,K = 5)$data
names(dataSet)[names(dataSet) == 'class'] <- 'Class'

run_plots(dataSet, "_balanced")

t <- divideDataset(dataSet) # 0 / 1
sampled <- sampleDataset(t[[1]], 7500) # zmien na 15000 / 7500
t0 <- sampled[[1]] 
t0_2 <- sampled[[2]]
t1 <- t[[2]]



runRRF(concatenateDatasets(t0, t1), t0_2, 
       ntree_list=list(10, 30, 50, 300, 400, 500), 
       mtry_list = list(10, 20, 400, 2000, 5000 ),
       maxnodes_list =list( 30, 50, 100, 150, 200))

 
tmp <- experimentSVM(concatenateDatasets(t0, t1), newSVMTwoC, folderName='testoutTwo', addValidDataset=t0_2, 
    gamma=list(4, 2, 0.5, 0.01, 0.001, 0.0001, 0.00001), nu=list(0.5, 0.3, 0.1, 0.01, 0.001))

tmp2 <- experimentSVM(t0, newSVMOneC, folderName='outOne', addValidDataset=concatenateDatasets(t0_2, t1), 
    gamma=list(4, 2, 0.5, 0.01, 0.001, 0.0001, 0.00001), nu=list(0.5, 0.3, 0.1, 0.01, 0.001))

#runOutForest(dataSet = concatenateDatasets(t0, t1), N= 5, extra_validation = t0_2,
#             max_n_outliers_list =  list(2, 10, 20, 50,100),
#             min_node_size_list =list(2, 10, 60, 100, 200))

#("out.csv", dataSet)

