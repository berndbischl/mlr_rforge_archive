test.resample = function() {
	cv.i = makeResampleInstance(makeResampleDesc("CV", iters=3), binaryclass.task)
	
	mylda = makeLearner("classif.lda", predict.type="prob")
	rf1 = resample("classif.lda", binaryclass.task, cv.i)$pred
	rf2 = resample(mylda, binaryclass.task, cv.i)$pred
	mylda = makeLearner("classif.lda", predict.type="prob")
	rf3 = resample(mylda, binaryclass.task, cv.i)$pred
  rf3 = setThreshold(rf3, 0)
	mylda = makeLearner("classif.lda", predict.type="prob")
	rf4 = resample(mylda, binaryclass.task, cv.i)$pred
  rf4 = setThreshold(rf4, 1)
  
	checkEquals(rf1@df$response, rf2@df$response)
	f1 = factor(rep(binaryclass.task["positive"], cv.i["size"]), levels=binaryclass.task["class.levels"])
	checkEquals(rf3@df$response, f1)
	f2 = factor(rep(binaryclass.task["negative"], cv.i["size"]), levels=binaryclass.task["class.levels"])
	checkEquals(rf4@df$response, f2)
	
	ct = makeClassifTask(data=iris[,c("Species", "Petal.Width")], target="Species")
	fit = resample("classif.lda", ct, makeResampleDesc("CV", iters=2))	
}



