source(here::here('scripts', 'utils.R'))
loadPackages()

set.seed(93274)
quit()

dataSet <- prepareData()
head(dataSet)

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

e <- ~ x + y + z
f <- y ~ x + b 
dataSet

e[[1]]
e[[2]]
f[[3]]

x <- list(10, 20, 30)
x[1]
x[[1]]
x[[1]]
typeof(x[1])
typeof(x[[1]])

unlist(labels(dataSet)[2])

#svm <- newSVMOne(x=dataSet, y=unlist(labels(dataSet)[2]), gamma=0.5, nu=0.5)
svm <- newSVMOne(x=x, y=y, gamma=0.5, nu=0.5)
# rpusvm(Class ~ ., data=dataSet, type='one-classification', kernel='radial', gamma=0.5, nu=0.5)

library(devtools)
devtools::install_github("h2oai/h2o4gpu", subdir = "src/interface_r")


getSummary("out.csv", dataSet)
# summary(dataSet)

terminate(FALSE)

#clear()
