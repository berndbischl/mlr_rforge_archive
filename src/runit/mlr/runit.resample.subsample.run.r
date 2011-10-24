
test.subsample.instance = function() {
  rin = makeResampleInstance(makeResampleDesc("Subsample", iters=2, split=0.25), size=20)
	
	iters <- rin@desc@iters
	checkEquals(iters, 2)
	
	
	for (i in 1:iters) {
    i1 = rin@train.inds[[i]]
    i2 = rin@test.inds[[i]]
    checkEquals(length(i1), 5)
		checkEquals(length(i2), 15)
		checkTrue(min(i1) >= 1)
		checkTrue(max(i1) <= 20)
		checkTrue(min(i2) >= 1)
		checkTrue(max(i2) <= 20)
		checkEquals(sort(c(i1, i2)), 1:20)
	}
  # check that resampling is really stochastic
  rin1 = makeResampleInstance(makeResampleDesc("Subsample", iters=3), size=500)
  rin2 = makeResampleInstance(makeResampleDesc("Subsample", iters=3), size=500)
  checkTrue(!all(sort(rin1@test.inds[[1]])== sort(rin2@test.inds[[1]])))
}
