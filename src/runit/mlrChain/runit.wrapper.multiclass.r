test.MulticlassWrapper <- function() {
  w = makeMulticlassWrapper("classif.logreg", mcw.method="onevsrest")
  res = makeResampleDesc("CV", iters=2)
  r = resample(w, multiclass.task, res)
  checkTrue(!any(is.na(r$aggr)))
  
  w = makeMulticlassWrapper("classif.logreg", mcw.method="onevsone")
  res = makeResampleDesc("CV", iters=2)
  r = resample(w, multiclass.task, res)
  checkTrue(!any(is.na(r$aggr)))
  
  cm = function(task) {
    m = matrix(1, 3, 3)
    diag(m) = -1
    rownames(m) = getClassLevels(task)
    m
  } 

  w = makeMulticlassWrapper("classif.lda", mcw.method=cm)
  res = makeResampleDesc("CV", iters=2)
  r = resample(w, multiclass.task, res)
  checkTrue(!any(is.na(r$aggr)))
  
}
