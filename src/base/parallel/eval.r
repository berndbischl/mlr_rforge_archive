
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

eval.rf <- function(learner, task, resampling, measures, aggr, parset, ps.scale, ps.names, vars) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (!is.null(parset)) {
		if (!is.null(ps.scale)) 
			parset = scale.par(ps.scale, parset)
		if (!is.null(ps.names)) 
			names(parset) = ps.names
	} else {
		parset = list()
	}
	if (is.null(vars)) {
		vars = task["input.names"]
	}
	st <- system.time(
			rr <- resample.fit(learner, task, resampling, parset=parset, vars=vars)
	)
	rp <- performance(rr, measures=measures, aggr=aggr, task=task)
	
	return(rp)
}

#eval.rf.perf <- function(learner, task, resampling, measures, aggr, parset, ps.scale, ps.names, vars) {
#	rp = eval.rf(learner, task, resampling, measures, aggr, parset, ps.scale, ps.names, vars)
#		
#	mm = rp$aggr
#	mm = reshape(mm, ids=row.names(mm), times=names(mm), varying=list(names(mm)), direction="long")
#	if(length(measures)==1)
#		rownames(mm) = paste(names(aggr), names(measures), sep=".")
#	mm = t(mm[,2, drop=FALSE])
#	return(mm)
#}


#eval.parsets <- function(learner, task, resampling, measures, aggr, pars, ps.scale, ps.names) {
#	ms = mylapply(xs=pars, from="tune", f=eval.rf.perf, 
#			learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, ps.scale=ps.scale, ps.names=ps.names, vars=NULL)
#	ps = par.list.to.df(pars)
#	ms = Reduce(rbind, ms)
#	y = cbind(ps, ms)
#	return(y)
#}
#
#eval.varsets <- function(learner, task, resampling, measures, aggr, varsets) {
#	rps = mylapply(xs=varsets, from="varsel", f=eval.rf, 
#			learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, parset=NULL, ps.scale=NULL, ps.names=NULL)
#	return(rps)
#}



#par.list.to.df = function(xs) {
#	y = Map(function(x) as.data.frame(x, stringsAsFactors=FALSE), xs)
#	Reduce(rbind, y)
#}
