test.resample.fit = function() {
	cv.i <- make.res.instance("cv", multiclass.task, iters=3)
	rf <- resample.fit("classif.lda", multiclass.task, cv.i, type="prob")
}



