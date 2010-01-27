
test.bs.instance = function() {
  rin <- make.res.instance("bs", size=25, iters=3)
  
  iters <- rin["iters"]
  checkEquals(iters, 3)

  for (i in 1:iters) {
    i1 <- rin["train.inds", i]
    i2 <- rin["test.inds", i]
    checkEquals(length(i1), 25)
    checkEquals(length(i2), 25 - length(unique(i1)))
    checkTrue(min(i1) >= 1)
    checkTrue(max(i1) <= 25)
    checkTrue(min(i2) >= 1)
    checkTrue(max(i2) <= 25)
    checkEquals(sort(c(unique(i1), i2)), 1:25)
  }

  A <- list()
  A[[1]] <- rin["train.inds", 1]
  A[[2]] <- rin["train.inds", 3]
  checkEquals(A, rin["train.inds", c(1,3)])

  A <- list()
  A[[1]] <- rin["test.inds", 1]
  A[[2]] <- rin["test.inds", 3]
  checkEquals(A, rin["test.inds", c(1,3)])

}




