test.multiclass.wrapper <- function() {
  w = make.multiclass.wrapper("classif.logreg", method="onevsrest")
  res = make.res.desc("cv", iters=2)
  r = resample(w, multiclass.task, res)
  checkTrue(!any(is.na(r$aggr)))
  
  w = make.multiclass.wrapper("classif.logreg", method="onevsone")
  res = make.res.desc("cv", iters=2)
  r = resample(w, multiclass.task, res)
  checkTrue(!any(is.na(r$aggr)))
  
  cm = function(task) {
    m = matrix(1, 3, 3)
    diag(m) = -1
    rownames(m) = task["class.levels"]
    m
  } 

  w = make.multiclass.wrapper("classif.logreg", method=cm)
  res = make.res.desc("cv", iters=2)
  r = resample(w, multiclass.task, res)
  checkTrue(!any(is.na(r$aggr)))
  
}
