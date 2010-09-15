
test.repcv.instance = function() {
  rin = make.res.instance("repcv", task=multiclass.task, iters=10, reps=3)
  
  folds = rin["iters"]
  checkEquals(folds, 10*3)
  reps = rin["reps"]
  checkEquals(reps, 10)
  
  for (j in 1:3) {
    bag =c()
    for (i in 1:10) {
      k = (j-1)*10 + i
      i1 = get.train.set(rin, k)
      i2 = get.test.set(rin, k)$inds
      checkEqual(length(unique(i1)), 135) 
      checkEqual(length(unique(i1)), 15) 
      bag = c(bag, i2)
    }
    checkEquals(sort(unique(bag)), 1:150)
}

