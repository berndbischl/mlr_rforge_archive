context("b632+")

test_that("b632+", {
  res = makeResampleDesc("Bootstrap", iters=3, predict="both")
  m = setAggregation(mmce, b632plus)
  r = resample(makeLearner("classif.lda"), multiclass.task, res, measures=m)
})