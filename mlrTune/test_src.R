library("devtools")
library("testthat")

load_all("skel")

library(mlbench)
task = makeClassifTask(data=iris, target="Species")
#lrn = makeLearner("classif.ksvm")
lrn = makeLearner("classif.lda", predict.type="prob")
rdesc = makeResampleDesc("Subsample", iters=30, split=4/5)
r = resample(lrn, task, rdesc)

m = train(lrn, task)
p = predict(m, task)

tr = tuneThreshold(r$pred, mmce)
print(tr)
#ps = makeParamSet(
#  makeDiscreteParam("C", values=c(1,2)),
#  makeDiscreteParam("sigma", values=c(1,2))
#)
#ps = makeParamSet(
#  makeNumericParam("C"),
#  makeNumericParam("sigma")
#)

#ctrl = makeTuneControlGrid()
#ctrl = makeTuneControlCMAES(start=list(sigma=20, C=10), maxit=2)
#ctrl = makeTuneControlOptim(start=list(sigma=20, C=10), maxit=2)
#print(ctrl)

#tr = tune(lrn, task, rdesc, par.set=ps, control=ctrl)
#print(tr)
#print(as.data.frame(tr$opt.path))


#ps = makeParamSet(
#  makeNumericVectorParam("cutoff", lower=0.0001, upper=1, length=3, 
#    trafo=function(x) x / (1.1*sum(x))), 
#  makeIntegerParam("ntree", lower=100, upper=500) 
#)

#ctrl = makeTuneControlCMAES(start=list(cutoff=c(1/3, 1/3, 1/3), ntree=200L), 
#  maxit=5, sigma=2)
#tr = tune(makeLearner("classif.randomForest"), task, rdesc, 
#  par.set=ps, control=ctrl)
