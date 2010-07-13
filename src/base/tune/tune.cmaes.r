

tune.cmaes = function(learner, task, resampling, measures, aggr, control) {
	
	path = list()
	
	g = function(p) {
		p2 = as.list(p)
		names(p2) = ns
		es = eval.state.tune(learner, task, resampling, measures, aggr, control, p2, "optim")
		path <<- add.path.tune(path, es, accept=T)		
		perf = get.perf(es)
		logger.info(level="tune", paste(ns, "=", p), ":", perf)
		return(perf)
	}
	
	args = control@extra.args
	
	ns = names(control["start"])
	start = as.numeric(control["start"])
	lower = as.numeric(control@lower)
	upper = as.numeric(control@upper)
	
	or = cma_es(par=start, fn=g, lower=lower, upper=upper, control=args)
	par = as.list(or$par)
	names(par) = ns
	opt = get.path.el(path, par)
	new("opt.result", control=control, opt=opt, path=path)
}
