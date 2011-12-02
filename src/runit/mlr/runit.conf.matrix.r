test.conf.matrix <- function() {
  res = makeResampleInstance(makeResampleDesc("CV", iters=3), task=multiclass.task)
  r = resample(makeLearner("classif.rpart"), multiclass.task, resampling = res)
	getConfMatrix(r$pred, relative = FALSE)
	getConfMatrix(r$pred, relative = TRUE)
}