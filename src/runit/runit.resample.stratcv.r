
test.stratcv.instance = function() {
  rin <- make.res.instance("stratcv", task=multiclass.task, iters=10)
  
  folds <- rin["iters"]
  checkEquals(folds, 10)
  
  for (i in 1:folds) {
    i1 = get.train.set(rin, i)
    i2 = get.test.set(rin, i)$inds
    checkTrue(all(as.numeric(table(multiclass.task["targets", row=i1])) == 45)) 
    checkTrue(all(as.numeric(table(multiclass.task["targets", row=i2])) == 5)) 
    checkEquals(sort(c(unique(i1), i2)), 1:150)
  }
}



