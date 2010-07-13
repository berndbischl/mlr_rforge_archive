tune.optim = function(learner, task, resampling, measures, aggr, control) {
	start = control["start"]
	ns = names(start)
	
	path = list()
	
	g = function(p) {
		p2 = as.list(p)
		es = eval.state.tune(learner, task, resampling, measures, aggr, control, p2, "optim")
		path <<- add.path.tune(path, es, accept=T)		
		perf = get.perf(es)
		logger.info(level="tune", paste(names(p), "=", p), ":", perf)
		return(perf)
	}
		
	par = as.list(optim(par=start, f=g, control=control@extra.args)$par)
	opt = get.path.el(path, par)
	new("opt.result", control=control, opt=opt, path=path)
}