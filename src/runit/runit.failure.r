test.failure <- function() {
	
	m <- train("qda", multiclass.task, subset=c(1,51,101))	
	checkTrue(!is.null(m["fail"]))
	p=predict(m, newdata=iris)
	checkTrue(all(is.na(p["response"])))
	
	
	wl = make.learner("kernlab.svm.regr", epsilon=10)
	m = train(wl, regr.task)	
	checkTrue(!is.null(m["fail"]))
	p=predict(m, newdata=regr.df)
	checkTrue(all(is.na(p["response"])))
	
}