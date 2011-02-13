
test.subsample.instance = function() {
  rin = make.res.instance(makeResampleDesc("subsample", iters=2, split=0.25), size=20)
	
	iters <- rin["iters"]
	checkEquals(iters, 2)
	
	
	for (i in 1:iters) {
    i1 = rin["train.inds"][[i]]
    i2 = rin["test.inds"][[i]]
    checkEquals(length(i1), 5)
		checkEquals(length(i2), 15)
		checkTrue(min(i1) >= 1)
		checkTrue(max(i1) <= 20)
		checkTrue(min(i2) >= 1)
		checkTrue(max(i2) <= 20)
		checkEquals(sort(c(i1, i2)), 1:20)
	}
}
