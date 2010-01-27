
test.cv.instance = function() {
  rin <- make.res.instance("cv", size=25, iters=3)

  folds <- rin["iters"]
  checkEquals(folds, 3)

  for (i in 1:folds) {
    i1 <- rin["train.inds", i]
    i2 <- rin["test.inds", i]
    checkTrue(min(i1) >= 1)
    checkTrue(max(i1) <= 25)
    checkTrue(min(i2) >= 1)
    checkTrue(max(i2) <= 25)
    checkEquals(sort(c(unique(i1), i2)), 1:25)
  }

  i <- rin["test.inds", 1:3]
  i <- melt.list(i)$value
  i <- sort(i)

  checkEquals(i, 1:25)


  A <- list()
  A[[1]] <- rin["train.inds", 1]
  A[[2]] <- rin["train.inds", 3]
  B <- rin["train.inds", c(1,3)]
  names(B) <- NULL
  checkEquals(A, B)

  A <- list()
  A[[1]] <- rin["test.inds", 1]
  A[[2]] <- rin["test.inds", 3]
  B <- rin["test.inds", c(1,3)]
  names(B) <- NULL
  checkEquals(A, B)

}




