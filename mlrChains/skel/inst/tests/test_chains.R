context("chains")

test_that("chains", {
  lrn1 = makeLearner("classif.rpart", minsplit=10)
  lrn2 = makePreprocWrapperRemoveOutliers(lrn1, ro.alpha=1)
  lrn3 = makePreprocWrapperPCA(lrn2)
  lrn4 = makeFilterWrapper(lrn3)
  m = train(lrn3, multiclass.task)
  
  p = predict(m, multiclass.task)
  perf = performance(p, mmce)
  expect_true(perf < 0.1)

  outer = makeResampleDesc("Holdout")
  inner = makeResampleDesc("CV", iters=2)

  ps = makeParamSet(
    makeDiscreteParam(id="minsplit", vals=c(5,10)),
    makeDiscreteParam(id="ro.alpha", vals=c(0.9, 1)),
    makeDiscreteParam(id="fw.perc", vals=c(0.8, 1))
  )
  
  lrn5 = makeTuneWrapper(lrn4, resampling=inner, par.set=ps2, 
    control=makeTuneControlOptim(start=list(C=0, epsilon=0, sigma=0), maxit=5))
  m = train(lrn5, task=multiclass.task)
  p = predict(m, task=multiclass.task)
    
  m = train(lrn2, task=regr.task)
  or = m$opt.result
  expect_equal(getOptPathLength(or$opt.path), 5+1)
  makeTuneWrapper(lrn)  
})

