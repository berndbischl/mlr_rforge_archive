test.FilterWrapper <- function() {
  w = makeFilterWrapper("classif.lda", fw.method="chi.squared", fw.threshold=-1)
  m = train(w, binaryclass.task)
  checkEquals(m["vars"], getFeatureNames(binaryclass.task))
  w = makeFilterWrapper("classif.lda", fw.method="chi.squared", fw.threshold=100000)
  m = train(w, binaryclass.task)
  checkEquals(m["vars"], character(0))
  checkTrue(is(m["learner.model"], "novars"))
  w = makeFilterWrapper("classif.lda", fw.method="chi.squared", fw.threshold=0.1)
  res = make.res.desc("cv", iters=2)
  r = resample(w, binaryclass.task, res)
  checkTrue(!any(is.na(r$aggr)))  
}
