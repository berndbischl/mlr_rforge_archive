## todo make path and parallel
#
#tune.ps <- function(learner, task, resampling, measures, aggr, control) {
#	print(resampling)
#	g = function(p) {
#		print(resampling)
#		rp = eval.rf(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, 
#				parset=p, ps.scale=scale, ps.names=names(control$start), vars=NULL)
#		rp$aggr[1,1]
#	} 
#	# use global path?
#	ps = pattern.search(f=g, control=control)
#	
##	
##	if (is.null(control))
##		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper)
##	else
##		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper, control=control)
##	par <- as.list(ps$par)
##	list(par=par, perf=ps$val, path=ps$path)
#	
#	path = list()
#	es = make.es(par=ps$par)
#	opt=make.path.el(es)
#	new("opt.result", opt=opt, path=path)
#}