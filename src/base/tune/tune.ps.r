tune.ps <- function(learner, task, resampling, loss, control, eval.fun) {
	g = function(p) eval.fun(p, resampling)
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