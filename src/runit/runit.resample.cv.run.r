
test.cv.instance = function() {
  rin <- make.res.instance(make.res.desc("cv", iters=3), size=25)

  folds <- rin["iters"]
  checkEquals(folds, 3)

  for (i in 1:folds) {
    i1 = rin["train.inds"][[i]]
    i2 = rin["test.inds"][[i]]
    checkTrue(min(i1) >= 1)
    checkTrue(max(i1) <= 25)
    checkTrue(min(i2) >= 1)
    checkTrue(max(i2) <= 25)
    checkEquals(sort(c(unique(i1), i2)), 1:25)
  }
}




