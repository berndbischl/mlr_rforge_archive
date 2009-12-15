test.resample.fit = function() {
	ct <- make.classif.task(data=multiclass.df, formula=multiclass.formula)
	cv.i <- make.cv.instance(nrow(multiclass.df), iters=3)
	rf <- resample.fit("lda", ct, cv.i, type="prob")
}



