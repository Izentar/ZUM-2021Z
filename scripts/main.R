source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)

dataSet <- prepareData()
#head(dataSet)


getSummary("out.csv", dataSet)
# summary(dataSet)

terminate(TRUE)

clear()


