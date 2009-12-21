test.conf.matrix <- function() {
	ct = make.classif.task(data=multiclass.df, target=multiclass.target)
	res = make.cv.instance(size=nrow(iris), iters=3)
	rf = resample.fit("lda", ct, resampling = res)
	conf.matrix(ct, rf, relative = FALSE)
	conf.matrix(ct, rf, relative = TRUE)
}