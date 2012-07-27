library(methods)
library(testthat)

  library(devtools)
  load_all("skel")

configureMlr(show.learner.output=FALSE)

  
  f = function(x) sum(x[[1]]^2) + (2 - x[[2]])^2
  
  ps = makeParamSet(
    makeNumericVectorParam("v", lower=-5, upper=5, length=2), 
    makeNumericParam("w", lower=-5, upper=5) 
  )
  learner = makeLearner("regr.randomForest")
  ctrl = makeMBOControl(init.design.points=10, seq.loops=10, propose.points.method="CMAES")
  or = mbo(f, ps, des=NULL, learner, ctrl)
  expect_true(!is.na(or$y))
  expect_equal(getOptPathLength(or$path), 15)
  ctrl = makeMBOControl(init.design.points=5, seq.loops=10, final.point="best.predicted")
  or = mbo(f, ps, des=NULL, learner, ctrl)
  expect_equal(getOptPathLength(or$path), 15)
