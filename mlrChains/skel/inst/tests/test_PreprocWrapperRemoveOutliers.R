context("PreprocWrapperRemoveOutliers")

test_that("PreprocWrapperRemoveOutliers", {
  lrn1 = makeLearner("classif.rpart", minsplit=10)
  lrn2 = makePreprocWrapperRemoveOutliers(lrn1, ro.alpha=1)
  m = train(lrn2, multiclass.task)  
  p = predict(m, multiclass.task)
  perf = performance(p, mmce)
  expect_equal(m$task.desc$size, 150)
  expect_true(perf < 0.1)
  lrn2 = makePreprocWrapperRemoveOutliers(lrn1, ro.alpha=1)
  
  lrn2 = setHyperPars(lrn2, ro.alpha=0.5)
  m = train(lrn2, multiclass.task)  
  p = predict(m, multiclass.task)
  # fixme: export getLeaf?
  if (interactive()) {
  expect_true(getLeafModel(m)$task.desc$size < 150)
  }
})

