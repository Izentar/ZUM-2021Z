source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)
#quit()

#
# Uwaga na zużycie pamięci w R. W moim przypadku procesy R nie zamknęły się nawet po zamknięciu RStudio.
# Trzeba to zrobić ręcznie.
#
#

dataSet <- prepareData(TRUE)


data(iris)
attach(iris)
iris
typeof(z)
x <- iris
y <- unlist(labels(iris)[2])
z <- Species ~ .
help("~")
print(Species ~ .)
typeof(Species ~ .)

#run_plots(dataSet, "_unbalanced")

dataSet <-  smotefamily::SMOTE(dataSet[, -31], dataSet$Class, K = 5)$data
 names(dataSet)[names(dataSet) == 'class'] <- 'Class'

#run_plots(dataSet, "_balanced")
runRRF(dataSet)

getSummary("out.csv", dataSet)
# summary(dataSet)

tmp <- 
experimentSVM(dataSet, newSVMTwoC, folderName='outTest', gamma=list(0.5, 0.6), nu=list(0.2))

#svm <- newSVMOneC(x=dataSet, y=unlist(labels(dataSet)[2]), gamma=0.5, nu=0.5)
#svm <- newSVMOneC(dataSet, gamma=0.5, nu=0.5)
# rpusvm(Class ~ ., data=dataSet, type='one-classification', kernel='radial', gamma=0.5, nu=0.5)
terminate(FALSE, TRUE)

library(devtools)
devtools::install_github("h2oai/h2o4gpu", subdir = "src/interface_r")


getSummary("out.csv", dataSet)
# summary(dataSet)

terminate(FALSE)

#clear()
clear()


data
data[data$class == 'owoc']
print(data$class == 'owoc')
