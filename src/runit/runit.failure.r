test.failure <- function() {
	
	m <- train("classif.qda", multiclass.task, subset=c(1,51,101))	
  checkTrue(is(m, "FailureModel"))
	checkTrue(!is.null(m@learner.model))
	p=predict(m, newdata=iris)
	checkTrue(all(is.na(p@df$response)))
	
	
	wl = makeLearner("regr.ksvm", epsilon=10)
	m = train(wl, regr.task)	
  checkTrue(is(m, "FailureModel"))
	checkTrue(!is.null(m@learner.model))
	p=predict(m, newdata=regr.df)
	checkTrue(all(is.na(p@df$response)))
	
}