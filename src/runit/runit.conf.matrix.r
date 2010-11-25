test.conf.matrix <- function() {
	res = make.res.instance("cv", multiclass.task, iters=3)
	rf = resample("classif.lda", multiclass.task, resampling = res)
	conf.matrix(rf, relative = FALSE)
	conf.matrix(rf, relative = TRUE)
}