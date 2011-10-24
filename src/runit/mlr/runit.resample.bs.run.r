
test.bs.instance = function() {
  rin <- makeResampleInstance(makeResampleDesc("BS", iters=3), size=25)
  
  iters <- rin@desc@iters
  checkEquals(iters, 3)

  for (i in 1:iters) {
    i1 = rin@train.inds[[i]]
    i2 = rin@test.inds[[i]]
    checkEquals(length(i1), 25)
    checkEquals(length(i2), 25 - length(unique(i1)))
    checkTrue(min(i1) >= 1)
    checkTrue(max(i1) <= 25)
    checkTrue(min(i2) >= 1)
    checkTrue(max(i2) <= 25)
    checkEquals(sort(c(unique(i1), i2)), 1:25)
  }
  # check that resampling is really stochastic
  rin1 = makeResampleInstance(makeResampleDesc("BS", iters=3), size=500)
  rin2 = makeResampleInstance(makeResampleDesc("BS", iters=3), size=500)
  checkTrue(!all(sort(rin1@test.inds[[1]])== sort(rin2@test.inds[[1]])))
}




