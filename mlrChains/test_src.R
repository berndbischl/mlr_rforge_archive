library("devtools")
library("testthat")
library(mlbench)
data(BostonHousing)

load_all("skel")

task = makeClassifTask(data=iris, target="Species")

lrn = makeLearner("classif.rpart", predict.type="prob")
par.set = makeParamSet(makeNumericParam("cp", lower=0.1, upper=0.8))
makeTuneWrapper(lrn, par.set=par.set)