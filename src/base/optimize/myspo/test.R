library(mlr)
source("src\\base\\optimize\\makeDesign.R")
source("src\\base\\optimize\\myspo\\control.myspo.opt.r")
source("src\\base\\optimize\\myspo\\proposePoints.R")
source("src\\base\\optimize\\myspo\\spo.helpers.r")
source("src\\base\\optimize\\myspo\\opt.meta.model.r")
source("src\\base\\optimize\\myspo\\myspo.r")

fun = function(x) {
  sum(unlist(x)^2)
}

ps = makeParameterSet(
  makeNumericParameter("x1", lower=-10, upper=10),
  makeNumericParameter("x2", lower=-10, upper=10)
)

set.seed(1)
des = makeDesign(30, ps, randomLHS, list())
des$y = evalDesign(des, fun)

ctrl = makeSPOControl(
  seq.loops=100, propose.points=1, 
  propose.points.method="seq.design", 
  seq.design.points=10000, seq.design.fun=randomLHS, seq.design.args=list()
)

opt.path = mlr:::makeOptimizationPath(names(ps@pars), "y", minimize=TRUE) 
myspo(fun, ps, des, "regr.randomForest", ctrl, opt.path)
plot(as.data.frame(opt.path)$y)

