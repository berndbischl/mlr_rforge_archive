library(methods)
library(testthat)

  library(devtools)
  load_all("skel")

configureMlr(show.learner.output=FALSE, on.learner.error="stop")


set.seed(2)

obj = function(x) { sum(x^2) }

ps = makeParamSet(
  makeNumericVectorParam("x", len=2, lower=-15, upper=15)
)

surrogate = makeLearner("regr.randomForest", ntree = 5)

ctrl = makeMBOControl(seq.loops = 2, init.design.points = 12, 
  final.point = "best.predicted",
  seq.design.points = 50,
  infill.opt = "CMAES"
)
res = mbo(makeMBOFunction(obj), ps, des=NULL, surrogate, ctrl)
print(res)

