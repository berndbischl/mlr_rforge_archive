library("devtools")
library("testthat")
library(parallelMap)

load_all("skel", reset=TRUE)
source("../mlrChains/skel/R/BaseWrapper.R")
source("../mlrChains/skel/R/BaseWrapper_operators.R")
source("../mlrChains/skel/R/ChainModel.R")
source("../mlrChains/skel/R/ChainModel_operators.R")

mydata = iris[, 1:4]
nalgos = 3
mycosts = matrix(rnorm(nrow(iris) * nalgos), ncol=nalgos)

task = makeCostSensTask(data=mydata, costs=mycosts)

lrn1 = makeLearner("regr.rpart")
lrn2 = makeCostSensRegrWrapper(lrn1)

#mod = train(lrn2, task)
#pred = predict(mod, task=task)
#p = performance(pred, measure=csm, task=task)
#print(p)
rdesc = makeResampleDesc("CV", iters=2)
r = resample(lrn2, task, rdesc, measure=csm)
