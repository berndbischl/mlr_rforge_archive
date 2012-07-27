library(methods)
library(testthat)

  library(devtools)
  load_all("skel")

configureMlr(show.learner.output=FALSE)

  
f1 = makeMBOFunction(function(x) {
  y = sum(x^2)
  if (y < 5)
    return(NA)
  return(y)
})
f2 = makeMBOFunction(function(x) {
  y = sum(x^2)
  if (y < 5)
    stop("foo")
  return(y)
})
ps = makeParamSet(
  makeNumericVectorParam("x", length=2, lower=0, upper=3)
)
learner = makeLearner("regr.randomForest")

ctrl = makeMBOControl(seq.loops=20, seq.design.points=500)
expect_error(mbo(f1, ps, des=NULL, learner, ctrl), "Infeasible y")
ctrl = makeMBOControl(seq.loops=20, seq.design.points=500, impute=function(x, y, opt.path) 0)
mbo(f1, ps, des=NULL, learner, ctrl)
ctrl = makeMBOControl(seq.loops=50, seq.design.points=500)
expect_error(mbo(f2, ps, des=NULL, learner, ctrl), "foo")
ctrl = makeMBOControl(seq.loops=50, seq.design.points=500, impute=function(x, y, opt.path) 0, impute.errors=TRUE)
mbo(f2, ps, des=NULL, learner, ctrl)