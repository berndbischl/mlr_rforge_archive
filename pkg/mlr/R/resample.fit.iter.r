#' @export
resample.fit.iter <- function(learner, learn.task, resample.instance, parset, vars, type, i, extract) {
	#print(i)
	train.i <- resample.instance["train.inds", i]
	test.i <- resample.instance["test.inds", i]
	m <- train(learner, learn.task, subset=train.i, parset=parset, vars=vars)
	if (is(learn.task, "classif.task"))
		p <- predict(m, newdata=learn.task["data", test.i], type=type)
	else 
		p <- predict(m, newdata=learn.task["data", test.i])
	# faster for parallel
	ex = extract(m)
	return(list(pred=p, extracted=ex))	
}
