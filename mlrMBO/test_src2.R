library(mlrChains)

library(methods)
library(testthat)

library(devtools)
load_all("skel")

configureMlr(show.learner.output=FALSE)


#set.seed(2)
#f1 = makeMBOFunction(function(x) 1)


runit = function(task, outer, inner, surrogate) {
  lrn1 = makeLearner("classif.ksvm")
  ps = makeParamSet(
    makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
    makeNumericParam("C", lower=-15, upper=15, trafo = function(x) 2^x)
    #makeNumericParam("sigma", lower=-15, upper=15, trafo = function(x) 2^x)
  )
  ppm = if (inherits(surrogate, "regr.km")) "EI" else "seq.design"
  sdp = if (inherits(surrogate, "regr.km"))  1 else 100
  ctrl = makeTuneControlMBO(learner = surrogate, mbo.control = makeMBOControl(
    seq.loops = 2, init.design.points = 10, 
    propose.points.method = ppm, seq.design.points = sdp,
    final.point = "best.predicted"
  ))
  lrn2 = makeTuneWrapper(lrn1, resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)
  resample(lrn2, task, outer)
}

library(mlbench)
data(Ionosphere)
#task = makeClassifTask(data = iris, target = "Species")
task = makeClassifTask(data = Ionosphere[,-2], target = "Class")
inner = makeResampleDesc("CV", iters = 2)
outer = makeResampleInstance(makeResampleDesc("CV", iters = 2), task = task)
surrogate = makeLearner("regr.randomForest", ntree = 50)
#surrogate = makeLearner("regr.km", nugget.estim = TRUE)
res = runit(task, outer, inner, surrogate)
print(res$aggr[1])
