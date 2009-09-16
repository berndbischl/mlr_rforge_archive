test.resample.fit = function() {
	ct <- make.classif.task("lda", data=multiclass.df, formula=multiclass.formula)
	cv.i <- make.cv.instance(nrow(multiclass.df), iters=3)
	rf <- resample.fit(ct, cv.i, type="prob")
}



