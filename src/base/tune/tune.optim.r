# todo: add optimize if only 1 par
tune.optim = function(learner, task, resampling, measures, control) {
  penv = new.env()
  ns = control["par.names"]
  start = unlist(control["start"])[ns]
  low = control["lower"]
  up = control["upper"]
	
	g = make.tune.f(ns, penv, learner, task, resampling, measures, control, p2, "optim")
		
	args = control@extra.args
	method = args$method
	if(is.null(method)) 
		method = "Nelder-Mead"
	args$method = NULL
	
  if (method == "L-BFGS-B") {
    or = optim(par=start, f=g, method=method, lower=low, upper=up, control=args)
  } else {
    or = optim(par=start, f=g, method=method, control=args)
  }
	par = as.list(or$par)
	opt = get.path.el(penv$path, par)
	new("opt.result", control=control, opt=opt, path=penv$path)
}
