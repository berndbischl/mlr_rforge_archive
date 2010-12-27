test.conf.matrix <- function() {
	res = make.res.instance("cv", multiclass.task, iters=3)
	r = resample("classif.rpart", multiclass.task, resampling = res)
	conf.matrix(r$pred, relative = FALSE)
	conf.matrix(r$pred, relative = TRUE)
}