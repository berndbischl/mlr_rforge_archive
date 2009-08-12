#' @export
resample.fit.iter <- function(learn.task, resample.instance, parset, type, i, return.model=FALSE) {
	#print(i)
	train.i <- resample.instance["train.inds", i]
	test.i <- resample.instance["test.inds", i]
	m <- train(learn.task, subset=train.i, parset=parset)
	if (is(learn.task, "classif.task"))
		p <- predict(learn.task, m, newdata=learn.task@data[test.i,], type=type)
	else 
		p <- predict(learn.task, m, newdata=learn.task@data[test.i,])
	# faster for parallel 
	if (!return.model)
		m <- NULL		
	return(list(pred=p, model=m))	
}
