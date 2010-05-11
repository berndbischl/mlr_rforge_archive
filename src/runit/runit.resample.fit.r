test.resample.fit = function() {
	cv.i <- make.res.instance("cv", binaryclass.task, iters=3)
	
	rf1 <- resample.fit("classif.lda", binaryclass.task, cv.i, type="response")
	rf2 <- resample.fit("classif.lda", binaryclass.task, cv.i, type="prob")
	rf3 <- resample.fit("classif.lda", binaryclass.task, cv.i, type="prob", threshold=0)
	rf4 <- resample.fit("classif.lda", binaryclass.task, cv.i, type="prob", threshold=1)
	
	checkEquals(rf1["response"], rf2["response"])
	f1 = factor(rep(binaryclass.task["positive"], cv.i["size"]), levels=binaryclass.task["class.levels"])
	checkEquals(rf3["response"], f1)
	f2 = factor(rep(binaryclass.task["negative"], cv.i["size"]), levels=binaryclass.task["class.levels"])
	checkEquals(rf4["response"], f2)
}



