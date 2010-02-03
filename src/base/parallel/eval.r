
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

eval.parset <- function(learner, task, resampling, loss, fixed, p, scale, names) {
	parset.scaled = scale.par(scale, p)
	
	names(parset.scaled) <- names
	parset = c(fixed, parset.scaled)
	st <- system.time(
			rr <- resample.fit(learner, task, resampling, parset)
	)
	rp <- resample.performance(task, rr, loss)
	
	logger.debug("parset ", as.character(parset))
	logger.debug("mean error = ", rp$aggr1)
	#logger.debug("Number of evaluations: ", n.eval)

	return(c(rp$aggr1, rp$spread, st["elapsed"]))
}

eval.parsets <- function(learner, task, resampling, loss, fixed, pars, scale, names) {
	zs = mylapply(pars, eval.parset, learner=learner, task=task, resampling=resampling, loss=loss, fixed=fixed, scale=scale, names=names) 
	z = t(as.data.frame(zs))
	colnames(z) = c("aggr", "spread", "time")
	rownames(z) = NULL
	return(z)
}
