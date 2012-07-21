library("devtools")
library("testthat")

load_all("skel")

task = makeClassifTask(data=iris, target="Species")
lrn = makeLearner("classif.ksvm")
rdesc = makeResampleDesc("Holdout")
ps = makeParamSet(
  makeDiscreteParam("C", values=c(1,2)),
  makeDiscreteParam("sigma", values=c(1,2))
)
ctrl = makeTuneControlGrid()
print(ctrl)
tr = tune(lrn, task, rdesc, par.set=ps, control=ctrl)