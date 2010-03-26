

tune.cmaes <- function(learner, task, resampling, measures, aggr, control, scale) {
	
	ns = names(control$start)
	
	g = function(p) {
		x = eval.rf.perf(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, parset=p, ps.scale=scale, ps.names=ns, vars=NULL)
		x[1]
	}
	
	s = unlist(control$start)
	control$start=NULL
	ps = cma_es(par=s, fn=g, control=control)
	names(ps$par) = ns
	list(par=ps$par, perf=ps$val)
}


