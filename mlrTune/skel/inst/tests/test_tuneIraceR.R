context("tuneIrace")

test_that("tuneIrace", {
  library(irace)
  res = makeResampleDesc("Holdout")
  ps1 = makeParamSet(
    makeNumericParam("cp", lower=0.001, upper=1), 
    makeIntegerParam("minsplit", lower=1, upper=10)
  )
  
  n = 100
  ctrl = makeTuneControlIrace(maxExperiments = n)
  tr1 = tune(makeLearner("classif.rpart"), multiclass.task, res, par.set=ps1, control=ctrl)
  expect_true(getOptPathLength(tr1$opt.path) >= 80 && getOptPathLength(tr1$opt.path) <= n)
  expect_true(!is.na(tr1$y))
 
  # with trafo
  res = makeResampleDesc("Holdout")
  ps2 = makeParamSet(
    makeNumericParam("C", lower=-5, upper=5, trafo=function(x) 2^x), 
    makeIntegerParam("sigma", lower=-5, upper=5, trafo=function(x) 2^x)
  )
  
  n = 100
  ctrl = makeTuneControlIrace(maxExperiments = n)
  tr2 = tune(makeLearner("classif.ksvm"), multiclass.task, res, par.set=ps2, control=ctrl)
  expect_true(getOptPathLength(tr2$opt.path) >= 80 && getOptPathLength(tr2$opt.path) <= n)
  expect_true(!is.na(tr2$y))
})



