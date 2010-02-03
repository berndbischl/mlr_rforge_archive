
#' @export
resample.fit.iter <- function(learner, task, rin, parset, vars, type, i, extract) {
	train.i <- rin["train.inds", i]
	test.i <- rin["test.inds", i]
	m <- train(learner, task, subset=train.i, parset=parset, vars=vars)
	if (is(task, "classif.task"))
		p <- predict(m, newdata=task["data", test.i], type=type)
	else 
		p <- predict(m, newdata=task["data", test.i])
	# faster for parallel
	ex = extract(m)
	return(list(pred=p, extracted=ex))	
}

eval.parset <- function(p, names, resampling) {
	parset.scaled   <- as.list(.mlr.scale(p))
	names(parset.scaled) <- names
	parset = c(.mlr.fixed, parset.scaled)
	
	st <- system.time(
			rr <- resample.fit(.mlr.learner, .mlr.task, resampling, parset)
	)
	rp <- resample.performance(.mlr.task, rr, .mlr.loss)
	
	logger.debug("parset ", as.character(parset))
	logger.debug("mean error = ", rp$aggr1)
	#logger.debug("Number of evaluations: ", n.eval)

	return(c(rp$aggr1, rp$spread, st["elapsed"]))
}

eval.parsets <- function(pars, names, resampling) {
	zs = lapply(pars, eval.parset, names, resampling)
	z = t(as.data.frame(zs))
	colnames(z) = c("aggr", "spread", "time")
	rownames(z) = NULL
	return(z)
}
