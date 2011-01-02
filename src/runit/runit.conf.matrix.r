test.conf.matrix <- function() {
  res = make.res.instance(make.res.desc("cv", iters=3), task=multiclass.task)
  r = resample("classif.rpart", multiclass.task, resampling = res)
	conf.matrix(r$pred, relative = FALSE)
	conf.matrix(r$pred, relative = TRUE)
}