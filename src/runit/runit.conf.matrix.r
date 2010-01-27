test.conf.matrix <- function() {
	res = make.res.instance("cv", multiclass.task, iters=3)
	rf = resample.fit("lda", multiclass.task, resampling = res)
	conf.matrix(multiclass.task, rf, relative = FALSE)
	conf.matrix(multiclass.task, rf, relative = TRUE)
}