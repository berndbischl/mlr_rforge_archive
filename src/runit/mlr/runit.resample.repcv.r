
test.repcv.instance = function() {
  rin = makeResampleInstance(makeResampleDesc("RepCV", iters=30, folds=10, reps=3), task=multiclass.task)
  
  iters = rin@desc@iters
  checkEquals(iters, 10*3)
  reps = rin@desc["reps"]
  checkEquals(reps, 3)
  
  for (j in 1:3) {
    bag =c()
    for (i in 1:10) {
      k = as.integer((j-1)*10L + i)
      i1 = rin["train.inds"][[i]]
      i2 = rin["test.inds"][[i]]
      checkEquals(length(unique(i1)), 135) 
      checkEquals(length(unique(i2)), 15) 
      bag = c(bag, i2)
    }
    checkEquals(sort(unique(bag)), 1:150)
  }
  # check that resampling is really stochastic
  rin1 = makeResampleInstance(makeResampleDesc("RepCV", folds=3, reps=2, iters=6), size=500)
  rin2 = makeResampleInstance(makeResampleDesc("RepCV", folds=3, reps=2, iters=6), size=500)
  checkTrue(!all(sort(rin1@test.inds[[1]])== sort(rin2@test.inds[[1]])))
  
  m = setAggregation(mmce, testgroup.mean)
  resample("classif.lda", multiclass.task, rin)
}
