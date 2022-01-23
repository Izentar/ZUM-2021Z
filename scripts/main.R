
sink(file = "output.log.txt", append=TRUE)

source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)
#quit()

#
# Uwaga na zużycie pamięci w R. W moim przypadku procesy R nie zamknęły się nawet po zamknięciu RStudio.
# Trzeba to zrobić ręcznie.
#
#

dataSet <- prepareData(FALSE, FALSE)

#run_plots(dataSet, "_unbalanced")

dataSet <-  smotefamily::SMOTE(dataSet[, -31], dataSet$Class, K = 5)$data
 names(dataSet)[names(dataSet) == 'class'] <- 'Class'

#run_plots(dataSet, "_balanced")
#runRRF(dataSet)

#getSummary("out.csv", dataSet)
# summary(dataSet)
 
tmp <- experimentSVM(dataSet, newSVMTwoC, folderName='outTwo', gamma=list(8, 4, 2, 1, 0.5, 0.01, 0.001, 0.0001), nu=list(0.5, 0.3, 0.15, 0.1, 0.05, 0.01))
tmp2 <- experimentSVM(dataSet, newSVMOneC, folderName='outOne', gamma=list(8, 4, 2, 1, 0.5, 0.01, 0.001, 0.0001), nu=list(0.5, 0.3, 0.15, 0.1, 0.05, 0.01))

#svm <- newSVMOneC(x=dataSet, y=unlist(labels(dataSet)[2]), gamma=0.5, nu=0.5)
#svm <- newSVMOneC(dataSet, gamma=0.5, nu=0.5)
# rpusvm(Class ~ ., data=dataSet, type='one-classification', kernel='radial', gamma=0.5, nu=0.5)
#terminate(FALSE, TRUE)

#library(devtools)
#devtools::install_github("h2oai/h2o4gpu", subdir = "src/interface_r")


#getSummary("out.csv", dataSet)
# summary(dataSet)

#terminate(FALSE)

#clear()
#clear()
