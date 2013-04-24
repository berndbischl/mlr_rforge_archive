library(methods)
library(testthat)

  library(devtools)
  load_all("skel")

configureMlr(show.learner.output=FALSE, on.learner.error="stop")


set.seed(2)

obj = function(x) {
  if (x$x1 == "a")
    x$x2^2
  else
    x$x2*4
}

ps = makeParamSet(
  makeDiscreteParam("x1", values=c("a", "b")),
  makeNumericParam("x2", lower=-15, upper=15)
)

surrogate = makeLearner("regr.randomForest", ntree = 5)
#surrogate = makeLearner("regr.km", nugget.estim = TRUE)

ctrl = makeMBOControl(seq.loops = 2, init.design.points = 12, 
  propose.points.method = "seq.design",
  final.point = "best.predicted"
)
res = mbo(obj, ps, des=NULL, surrogate, ctrl)
print(res$x)
print(res$y)

