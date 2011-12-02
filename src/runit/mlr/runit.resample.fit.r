test.resample = function() {
	cv.i = makeResampleInstance(makeResampleDesc("CV", iters=3), binaryclass.task)
	
	lrn1 = makeLearner("classif.lda")
	lrn2 = makeLearner("classif.lda", predict.type="prob")
	rf1 = resample(lrn1, binaryclass.task, cv.i)$pred
	rf2 = resample(lrn2, binaryclass.task, cv.i)$pred
	rf3 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf3 = setThreshold(rf3, 0)
	rf4 = resample(lrn2, binaryclass.task, cv.i)$pred
  rf4 = setThreshold(rf4, 1)
  
	checkEquals(rf1@df$response, rf2@df$response)
	f1 = factor(rep(binaryclass.task@desc@positive, cv.i@size), levels=binaryclass.task@desc@class.levels)
	checkEquals(rf3@df$response, f1)
	f2 = factor(rep(binaryclass.task@desc@negative, cv.i@size), levels=binaryclass.task@desc@class.levels)
	checkEquals(rf4@df$response, f2)
	
	ct = makeClassifTask(data=iris[,c("Species", "Petal.Width")], target="Species")
	fit = resample(lrn1, ct, makeResampleDesc("CV", iters=2))
}



