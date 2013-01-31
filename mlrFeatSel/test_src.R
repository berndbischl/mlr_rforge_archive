library("devtools")
library("testthat")
library(mlbench)
library(mlr)
data(BostonHousing)

load_all("skel")

task = makeClassifTask(data=iris, target="Species")
lrn = makeLearner("classif.rpart")
rdesc = makeResampleDesc("Holdout")

ctrl = makeFeatSelControlRandom(max.features=2, maxit=10)

or = selectFeatures(lrn, task, rdesc, control=ctrl)
print(or)
#z = filterFeatures(task)
#print(z)

#mod = train(lrn2, task)

#print(getHyperPars(lrn2))
#lrn2 = setHyperPars(lrn2, minsplit=12)
#print(getHyperPars(lrn2))


