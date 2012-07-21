library("devtools")
library("testthat")

load_all("skel")

task = makeClassifTask(data=iris, target="Species")
lrn = makeLearner("classif.ksvm")
rdesc = makeResampleDesc("Subsample", iters=10)

#ps = makeParamSet(
#  makeDiscreteParam("C", values=c(1,2)),
#  makeDiscreteParam("sigma", values=c(1,2))
#)
ps = makeParamSet(
  makeNumericParam("C"),
  makeNumericParam("sigma")
)

#ctrl = makeTuneControlGrid()
#ctrl = makeTuneControlCMAES(start=list(sigma=20, C=10), maxit=2)
ctrl = makeTuneControlOptim(start=list(sigma=20, C=10), maxit=2)
print(ctrl)

tr = tune(lrn, task, rdesc, par.set=ps, control=ctrl)
print(tr)
print(as.data.frame(tr$opt.path))