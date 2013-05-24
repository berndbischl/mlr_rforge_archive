library(methods)
library(testthat)
library(devtools)
library(cmaes)
load_all("skel", reset=TRUE)
configureMlr(show.learner.output=FALSE, on.learner.error="stop")

set.seed(2)

obj = function(x) { sum(x^2) }

ps = makeParamSet(
  makeNumericVectorParam("x", len=2, lower=-15, upper=15)
)

#surrogate = makeLearner("regr.randomForest", ntree = 5)
surrogate = makeLearner("regr.km", predict.type="se")

ctrl = makeMBOControl(seq.loops = 2, init.design.points = 12, 
  final.point = "best.predicted",
  #seq.design.points = 50,
  infill.crit = "mean",
  infill.opt = "cmaes",
  cmaes.control = list(maxit = 5)                      
)
res = mbo(makeMBOFunction(obj), ps, des=NULL, surrogate, ctrl)
print(res)

