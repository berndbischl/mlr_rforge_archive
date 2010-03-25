tune.nm <- function(learner, task, resampling, measures, aggr, control, scale) {
	
	ns = names(control$start)
	
	g = function(p) {
		x = eval.rf.perf(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, parset=p, ps.scale=scale, ps.names=ns, vars=NULL)
		x[1]
	}
	
	s = control$start
	control$start=NULL
	ps = optim(par=s, f=g, control=control)
	
#	
#	if (is.null(control))
#		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper)
#	else
#		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper, control=control)
#	par <- as.list(ps$par)
#	list(par=par, perf=ps$val, path=ps$path)
	list(par=ps$par, perf=ps$val, path=ps$path)
}