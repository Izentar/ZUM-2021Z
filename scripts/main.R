source(here::here('scripts', 'utils.R'))
loadPackages()

dataSet <- prepareData()
#head(dataSet)
getSummary("out.txt", dataSet)

terminate()
