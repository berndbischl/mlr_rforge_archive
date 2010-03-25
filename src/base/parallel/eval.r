
#' @export
resample.fit.iter <- function(learner, task, rin, vars, type, i, extract) {
	train.i <- rin["train.inds", i]
	test.i <- rin["test.inds", i]
	m <- train(learner, task, subset=train.i, vars=vars)
	if (is(task, "classif.task"))
		p <- predict(m, task=task, subset=test.i, type=type)
	else 
		p <- predict(m, task=task, subset=test.i)
	# faster for parallel
	ex = extract(m)
	return(list(pred=p, extracted=ex))	
}

eval.rf <- function(learner, task, resampling, measures, aggr, parset, scale, names, vars) {
	if (is.character(learner))
		learner = make.learner(learner, task=task)
	if (!missing(parset)) {
		parset = scale.par(scale, parset)
		names(parset) = names
	}
	st <- system.time(
			rr <- resample.fit(learner, task, resampling, parset=parset, vars=vars)
	)
	rp <- performance(rr, measures=measures, aggr=aggr)
	#.mlr.local$n.eval <<- .mlr.local$n.eval+1 
	#print(.mlr.local$n.eval)
	#logger.debug("parset ", as.character(parset))
	#logger.debug("mean error = ", rp$aggr1)
	#logger.debug("Number of evaluations: ", n.eval)
	
	
	mm = rp$measures[names(aggr), names(measures), drop=FALSE]
	mm = reshape(mm, ids=row.names(mm), times=names(mm), varying=list(names(mm)), direction="long")
	if(length(measures)==1)
		rownames(mm) = paste(names(aggr), names(measures), sep=".")
	
	feval = try(get(".mlr.feval", envir=.GlobalEnv))
	if (is(feval, "try-error"))
		feval=0
	assign(".mlr.feval", feval+1, envir=.GlobalEnv)
	
	mm = t(mm[,2, drop=FALSE])
	return(mm)
}

eval.parsets <- function(learner, task, resampling, measures, aggr, pars, scale, names) {
	ms = mylapply(xs=pars, from="tune", f=function(p) 
		eval.rf(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, parset=p, scale=scale, names=names))
	ps = par.list.to.df(pars)
	ms = Reduce(rbind, ms)
	y = cbind(ps, ms)
	return(y)
}

eval.varsets <- function(learner, task, resampling, measures, aggr, vars) {
	ms = mylapply(xs=pars, from="tune", f=function(v) 
		eval.rf(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, vars=v))
	#ps = par.list.to.df(pars)
	#ms = Reduce(rbind, ms)
	#y = cbind(ps, ms)
	return(ms)
}



par.list.to.df = function(xs) {
	y = Map(function(x) as.data.frame(x, stringsAsFactors=FALSE), xs)
	Reduce(rbind, y)
}
