

tune.cmaes = function(learner, task, resampling, measures, aggr, control) {
	
	path = list()
	
	g = function(p) {
		p2 = as.list(p)
		names(p2) = ns
		es = eval.state.tune(learner, task, resampling, measures, aggr, control, p2, "optim")
		path <<- add.path.tune(path, es, accept=TRUE)		
		perf = get.perf(es)
		logger.info(level="tune", paste(ns, "=", p), ":", perf)
		return(perf)
	}
	
	args = control@extra.args
	
	ns = names(control["start"])
	start = as.numeric(control["start"])
	
	or = cma_es(par=start, fn=g, lower=control["lower"], upper=control["upper"], control=args)
	par = as.list(or$par)
	names(par) = ns
	opt = get.path.el(path, par)
	new("opt.result", control=control, opt=opt, path=path)
}
