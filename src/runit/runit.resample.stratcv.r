
test.stratcv.instance = function() {
  rin = make.res.instance(makeResampleDesc("stratcv", iters=10), task=multiclass.task)  
  
  folds = rin["iters"]
  checkEquals(folds, 10)
  
  for (i in 1:folds) {
    i1 = rin["train.inds"][[i]]
    i2 = rin["test.inds"][[i]]
    checkTrue(all(as.numeric(table(multiclass.task["targets"][i1])) == 45)) 
    checkTrue(all(as.numeric(table(multiclass.task["targets"][i2])) == 5)) 
    checkEquals(sort(c(unique(i1), i2)), 1:150)
  }
}



