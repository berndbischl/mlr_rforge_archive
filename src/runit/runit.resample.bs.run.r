
test.bs.instance = function() {
  rin <- make.res.instance("bs", size=25, iters=3)
  
  iters <- rin["iters"]
  checkEquals(iters, 3)

  for (i in 1:iters) {
    i1 <- get.train.set(rin, i)
    i2 <- get.test.set(rin, i)$inds
    checkEquals(length(i1), 25)
    checkEquals(length(i2), 25 - length(unique(i1)))
    checkTrue(min(i1) >= 1)
    checkTrue(max(i1) <= 25)
    checkTrue(min(i2) >= 1)
    checkTrue(max(i2) <= 25)
    checkEquals(sort(c(unique(i1), i2)), 1:25)
  }
}




