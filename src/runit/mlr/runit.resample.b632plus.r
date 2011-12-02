test.b632plus <- function() {
  res = makeResampleDesc("BS", iters=3, predict="both")
  m = setAggregation(mmce, b632plus)
  r = resample(makeLearner("classif.lda"), multiclass.task, res, measures=m)
}