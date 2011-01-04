test.b632plus <- function() {
  res = make.res.desc("bs", iters=3, predict="both")
  m = set.aggr(mmce, b632plus)
  r = resample("classif.lda", multiclass.task, res, measures=m)
}