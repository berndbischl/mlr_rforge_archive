test.failure <- function() {
	
	ct <- make.classif.task("qda", data=multiclass.df, formula=multiclass.formula)
	m <- train(ct, subset=c(1,51,101))	
	checkTrue(!is.null(m["fail"]))
	p=predict(m, newdata=iris)
	checkTrue(all(is.na(p)))
}