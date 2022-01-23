
sink(file = "output.log.txt", append=TRUE)

source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)


dataSet <- prepareData(FALSE, FALSE)
#print(dataSet)
#run_plots(dataSet, "_unbalanced")

dataSet = subset(dataSet, select = -c(V15,V7, V18, V8, V1, V5, V2, V28, V25, V23, V27))
dataSet <-  SMOTE(dataSet[,-20], dataSet$Class,K = 5)$data
names(dataSet)[names(dataSet) == 'class'] <- 'Class'

t <- divideDataset(dataSet) # 0 / 1
ttt <- sampleDataset(t[[1]], 17000) # zmien na 10000
t0 <- ttt[[1]] 
t0_2 <- ttt[[2]]
t1 <- t[[2]]

#control <- caret::trainControl(method="repeatedcv", number=5, repeats=1)
#model <- caret::train(Class~., data=dataSet, method="rf", trControl=control, importance = TRUE)
#library(randomForest)
 
#model <- randomForest(factor(Class)~., data=dataSet,  importance = TRUE)

#importance(model)
#varImpPlot(model)



#run_plots(dataSet, "_unbalanced")





#print(importance)
#plot(importance)



#run_plots(dataSet, "_balanced")
#runRRF(concatenateDatasets(t0, t1), t0_2)


#getSummary("out.csv", dataSet)
# summary(dataSet)

# dataSet 20 000 + ..

#print(t0)

#tmp <- experimentSVM(concatenateDatasets(t0, t1), newSVMTwoC, folderName='outTwo', addValidDataset=t0_2, 
#                     gamma=list(0.5), nu=list(0.1))
 
tmp <- experimentSVM(concatenateDatasets(t0, t1), newSVMTwoC, folderName='outTwo', addValidDataset=t0_2, 
    gamma=list(4, 2, 0.5, 0.01, 0.001, 0.0001, 0.00001), nu=list(0.5, 0.3, 0.1, 0.01, 0.001))

tmp2 <- experimentSVM(t0, newSVMOneC, folderName='outOne', addValidDataset=concatenateDatasets(t0_2, t1), 
    gamma=list(4, 2, 0.5, 0.01, 0.001, 0.0001, 0.00001), nu=list(0.5, 0.3, 0.1, 0.01, 0.001))



#dataSet <- prepareData(TRUE, FALSE)

#t <- divideDataset(dataSet) # 0 / 1
#ttt <- sampleDataset(t[[1]], 2500)
#t0 <- ttt[[1]]
#t0_2 <- ttt[[0]]
#t1 <- t[[2]]

#runOutForest(concatenateDatasets(t0, t1), t0_2)
#getSummary("out.csv", dataSet)

