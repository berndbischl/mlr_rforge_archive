
test.repcv.instance = function() {
  rin = make.res.instance("repcv", task=multiclass.task, iters=10, reps=3)
  
  folds = rin["iters"]
  checkEquals(folds, 10*3)
  reps = rin["reps"]
  checkEquals(reps, 3)
  
  for (j in 1:3) {
    bag =c()
    for (i in 1:10) {
      k = as.integer((j-1)*10L + i)
      i1 = get.train.set(rin, k)
      i2 = get.test.set(rin, k)
      checkEquals(length(unique(i1)), 135) 
      checkEquals(length(unique(i2)), 15) 
      bag = c(bag, i2)
    }
    checkEquals(sort(unique(bag)), 1:150)
  }
}

