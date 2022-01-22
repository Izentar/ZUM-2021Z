source(here::here('scripts', 'utils.R'))


loadPackages()

set.seed(93274)

dataSet <- prepareData()

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






runOutForest(dataSet)
getSummary("out.csv", dataSet)
# summary(dataSet)

#terminate(TRUE)

