context("tuneRandom")

test_that("tuneRandom", {
  lrn = makeLearner("classif.ksvm")
  res = makeResampleDesc("Holdout")
  ps1 = makeParamSet(
    makeNumericParam("C", lower=0.001, upper=1), 
    makeDiscreteParam("kernel", values=c("rbfdot", "vanilladot"))
  )
  
  ctrl = makeTuneControlRandom(maxit=5)
  tr1 = tune(lrn, multiclass.task, res, par.set=ps1, control=ctrl)
  expect_equal(getOptPathLength(tr1$opt.path), 5)
  expect_true(!is.na(tr1$y))
})

