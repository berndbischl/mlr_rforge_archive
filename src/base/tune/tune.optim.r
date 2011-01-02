# todo: add optimize if only 1 par
tune.optim = function(learner, task, resampling, measures, control) {
  path = list()
  ns = control["par.names"]
  start = unlist(control["start"])[ns]
  low = control["lower"]
  up = control["upper"]
	
	g = function(p) {
    p2 = as.list(p)
    names(p2) = ns
		es = eval.state.tune(learner, task, resampling, measures, control, p2, "optim")
		path <<- add.path.tune(path, es, accept=TRUE)		
		perf = get.perf(es)
		logger.info(level="tune", paste(ns, "=", p), ":", perf)
		return(perf)
	}
		
	args = control@extra.args
	method = args$method
	if(is.null(method)) 
		method = "Nelder-Mead"
	args$method = NULL
	
  if (method == "L-BFGS-B") {
    or = optim(par=start, f=g, method=method, lower=low, upper=up, control=args)
  } else 
    or = optim(par=start, f=g, method=method, control=args)
  }
	par = as.list(or$par)
	opt = get.path.el(path, par)
	new("opt.result", control=control, opt=opt, path=path)
}
