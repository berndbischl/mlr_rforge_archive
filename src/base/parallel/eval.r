
#' @export
resample.fit.iter <- function(learner, task, rin, parset, vars, type, i, extract) {
	train.i <- rin["train.inds", i]
	test.i <- rin["test.inds", i]
	m <- train(learner, task, subset=train.i, parset=parset, vars=vars)
	if (is(task, "classif.task"))
		p <- predict(m, task=task, subset=test.i, type=type)
	else 
		p <- predict(m, task=task, subset=test.i)
	# faster for parallel
	ex = extract(m)
	return(list(pred=p, extracted=ex))	
}

eval.parset <- function(learner, task, resampling, measures, aggr, fixed, p, scale, names) {
	parset.scaled = scale.par(scale, p)
	
	names(parset.scaled) <- names
	parset = c(fixed, parset.scaled)
	st <- system.time(
			rr <- resample.fit(learner, task, resampling, parset)
	)
	rp <- resample.performance(task, rr, measures=measures, aggr=aggr)
	logger.debug("parset ", as.character(parset))
	#logger.debug("mean error = ", rp$aggr1)
	#logger.debug("Number of evaluations: ", n.eval)
	
	
	mm = rp$measures[names(aggr), names(measures), drop=FALSE]
	mm = reshape(mm, ids=row.names(mm), times=names(mm), varying=list(names(mm)), direction="long")
	if(length(measures)==1)
		rownames(mm) = paste(names(aggr), names(measures), sep=".")
		
	mm = t(mm[,2, drop=FALSE])
	return(mm)
}

eval.parsets <- function(learner, task, resampling, measures, aggr, fixed, pars, scale, names) {
	ms = mylapply(pars, eval.parset, from="tune", learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, fixed=fixed, scale=scale, names=names)
	ps = par.list.to.df(pars)
	ms = Reduce(rbind, ms)
	y = cbind(ps, ms)
	return(y)
}

par.list.to.df = function(xs) {
	y = Map(function(x) as.data.frame(x, stringsAsFactors=FALSE), xs)
	Reduce(rbind, y)
}
