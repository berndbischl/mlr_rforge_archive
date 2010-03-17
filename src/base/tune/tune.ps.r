tune.ps <- function(learner, task, resampling, measures, aggr, control, fixed, scale) {
	g = function(p) eval.parset(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, fixed=fixed, p, scale=scale, names=names(control$start))[1]
	ps = pattern.search(f=g, control=control)
	
#	
#	if (is.null(control))
#		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper)
#	else
#		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper, control=control)
#	par <- as.list(ps$par)
#	list(par=par, perf=ps$val, path=ps$path)
	list(par=ps$par, perf=ps$val, path=ps$path)
}