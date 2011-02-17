test.b632plus <- function() {
  res = makeResampleDesc("BS", iters=3, predict="both")
  m = setAggr(mmce, b632plus)
  r = resample("classif.lda", multiclass.task, res, measures=m)
}