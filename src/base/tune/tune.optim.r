tune.optim = function(learner, task, resampling, measures, aggr, control) {
	start = control["start"]
	ns = names(start)
	
	path = list()
	path = add.path.tune(path, state, accept=T)		
	
	g = function(p) {
		print(str(p))
		es = eval.state.tune(learner, task, resampling, measures, aggr, control, p, "optim")
		path <<- add.path.tune(path, es, accept=T)		
		get.perf(es) 
	}
	
	ps = optim(par=start, f=g, maxit=5)
	bs = make.path.el
	new("opt.result", control=control, opt=make.path.el(bs), path=path)
}