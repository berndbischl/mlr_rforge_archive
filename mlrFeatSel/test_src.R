#library("devtools")
#library("testthat")
#library(mlbench)
#data(BostonHousing)

#load_all("skel")

#task = makeClassifTask(data=iris, target="Species")
#lrn = makeLearner("classif.rpart")
#rdesc = makeResampleDesc("Holdout")

#ctrl = makeTuneControlIrace()
# 
# ps = makeParameterSet(
#   makeNumericParameter("cp", lower=0.1, upper=0.2),
#   makeIntegerParameter("minsplit", lower=5, upper=10)
# )
# 
#tr = tune(lrn, task, rdesc, control=ctrl, par.set=ps)

#ctrl = makeFeatSelControlRandom(maxit=NA)

#or = selectFeatures(lrn, task, rdesc, control=ctrl)
#print(or)
#z = filterFeatures(task)
#print(z)

#mod = train(lrn2, task)

#print(getHyperPars(lrn2))
#lrn2 = setHyperPars(lrn2, minsplit=12)
#print(getHyperPars(lrn2))


