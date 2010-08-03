
test.cv.instance = function() {
  rin <- make.res.instance("cv", size=25, iters=3)

  folds <- rin["iters"]
  checkEquals(folds, 3)

  for (i in 1:folds) {
    i1 = get.train.set(rin, i)
    i2 = get.test.set(rin, i)$inds
    checkTrue(min(i1) >= 1)
    checkTrue(max(i1) <= 25)
    checkTrue(min(i2) >= 1)
    checkTrue(max(i2) <= 25)
    checkEquals(sort(c(unique(i1), i2)), 1:25)
  }
}




