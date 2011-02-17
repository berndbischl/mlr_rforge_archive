test.conf.matrix <- function() {
  res = makeResampleInstance(makeResampleDesc("CV", iters=3), task=multiclass.task)
  r = resample("classif.rpart", multiclass.task, resampling = res)
	conf.matrix(r$pred, relative = FALSE)
	conf.matrix(r$pred, relative = TRUE)
}