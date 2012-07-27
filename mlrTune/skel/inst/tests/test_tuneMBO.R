context("tuneMBO")

test_that("tuneMBO", {
  res = makeResampleDesc("CV", iters=2)
  ps1 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeIntegerParam("minsplit", lower=1, upper=10)
  )
  
  mbo.ctrl = makeMboControl(init.design.points=3, seq.loops=2)
  ctrl = makeTuneControlMbo(learner=makeLearner("regr.randomForest"), mbo.control=mbo.ctrl)
  tr1 = tune(makeLearner("classif.rpart"), multiclass.task, res, par.set=ps1, control=ctrl)
  expect_equal(getOptPathLength(tr1$opt.path), 5)
  expect_equal(dim(as.data.frame(tr1$opt.path)), c(5, 2+1+2))
  
  ps2 = makeParamSet(
    makeIntegerParam("ntree", lower=100, upper=500),
    makeNumericVectorParam("cutoff", length=3, lower=0.001, upper=1, trafo=function(x) 0.9*x/sum(x)) 
  )
  tr2 = tune(makeLearner("classif.randomForest"), multiclass.task, res, par.set=ps2, control=ctrl)
  expect_equal(getOptPathLength(tr2$path), 5)
})




