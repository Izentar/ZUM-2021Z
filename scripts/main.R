
sink(file = "output.log.txt", append=TRUE)

source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)

dataSet <- prepareData(TRUE)

dataSet <- prepareData(TRUE, TRUE)
run_plots(dataSet, "_unbalanced")
dataSet <-  SMOTE(dataSet[,-31], dataSet$Class,K = 5)$data


control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Class~., data=dataSet, method="RRF", trControl=control)
importance <- varImpPlot(model, scale=FALSE)
print(importance)
plot(importance)



names(dataSet)[names(dataSet) == 'class'] <- 'Class'

run_plots(dataSet, "_balanced")
runRRF(dataSet)


#getSummary("out.csv", dataSet)
# summary(dataSet)

# dataSet 20 000 + ..
t <- divideDataset(dataSet) # 0 / 1
ttt <- sampleDataset(t[[1]], 10000)
t0 <- ttt[[1]]
t0_2 <- ttt[[0]]
t1 <- t[[2]]
 
tmp <- experimentSVM(concatenateDatasets(t0, t1), newSVMTwoC, folderName='outTwo', addValidDataset=t0_2, 
    gamma=list(8, 4, 2, 1, 0.5, 0.01, 0.001, 0.0001), nu=list(0.5, 0.3, 0.15, 0.1, 0.05, 0.01))

tmp2 <- experimentSVM(t0, newSVMOneC, folderName='outOne', addValidDataset=concatenateDatasets(t0_2, t1), 
    gamma=list(8, 4, 2, 1, 0.5, 0.01, 0.001, 0.0001), nu=list(0.5, 0.3, 0.15, 0.1, 0.05, 0.01))




runOutForest(dataSet)
getSummary("out.csv", dataSet)
# summary(dataSet)

#terminate(TRUE)

