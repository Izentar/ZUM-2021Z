source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)

dataSet <- prepareData(TRUE)



#run_plots(dataSet, "_unbalanced")

dataSet <-  smotefamily::SMOTE(dataSet[, -31], dataSet$Class, K = 5)$data
 names(dataSet)[names(dataSet) == 'class'] <- 'Class'

#run_plots(dataSet, "_balanced")
runRRF(dataSet)

getSummary("out.csv", dataSet)
# summary(dataSet)

terminate(FALSE, TRUE)

clear()


data
data[data$class == 'owoc']
print(data$class == 'owoc')
