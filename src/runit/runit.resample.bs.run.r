
test.bs.instance = function() {
  rin <- make.res.instance(make.res.desc("bs", iters=3), size=25)
  
  iters <- rin["iters"]
  checkEquals(iters, 3)

  for (i in 1:iters) {
    i1 = rin["train.inds"][[i]]
    i2 = rin["test.inds"][[i]]
    checkEquals(length(i1), 25)
    checkEquals(length(i2), 25 - length(unique(i1)))
    checkTrue(min(i1) >= 1)
    checkTrue(max(i1) <= 25)
    checkTrue(min(i2) >= 1)
    checkTrue(max(i2) <= 25)
    checkEquals(sort(c(unique(i1), i2)), 1:25)
  }
}




