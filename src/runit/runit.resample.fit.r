test.resample = function() {
	cv.i <- make.res.instance("cv", binaryclass.task, iters=3)
	
	mylda = make.learner("classif.lda", predict.type="prob")
	rf1 = resample("classif.lda", binaryclass.task, cv.i)
	rf2 = resample(mylda, binaryclass.task, cv.i)
	mylda = make.learner("classif.lda", predict.type="prob", predict.threshold=0)
	rf3 = resample(mylda, binaryclass.task, cv.i)
	mylda = make.learner("classif.lda", predict.type="prob", predict.threshold=1)
	rf4 = resample(mylda, binaryclass.task, cv.i)
	
	checkEquals(rf1["response"], rf2["response"])
	f1 = factor(rep(binaryclass.task["positive"], cv.i["size"]), levels=binaryclass.task["class.levels"])
	checkEquals(rf3["response"], f1)
	f2 = factor(rep(binaryclass.task["negative"], cv.i["size"]), levels=binaryclass.task["class.levels"])
	checkEquals(rf4["response"], f2)
	
	ct = make.task(data=iris[,c("Species", "Petal.Width")], target="Species")
	fit = resample("classif.lda", ct, make.res.desc("cv", iters=2))	
}



