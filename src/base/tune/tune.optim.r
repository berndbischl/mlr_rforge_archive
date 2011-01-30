# todo: add optimize if only 1 par
tune.optim = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  if (any(sapply(par.set@pars, function(x) !(x@type %in% c("numeric", "integer")))))
    stop("Optim can only be applied to numeric and integer parameters!")
  
  start = unlist(control@start)
  low = lower(par.set)
  upp = upper(par.set)
	
	g = make.tune.f(learner, task, resampling, measures, par.set, control, opt.path, log.fun)
		
	args = control@extra.args
	method = args$method
	if(is.null(method)) 
		method = "Nelder-Mead"
	args$method = NULL
  
  if (method == "L-BFGS-B") {
    or = optim(par=start, f=g, method=method, lower=low, upper=upp, control=args)
  } else {
    if (any((is.double(low) & low != -Inf) | (is.integer(low) & low != -.Machine$integer.max)) ||
        any((is.double(upp) & upp !=  Inf) | (is.integer(upp) & upp !=  .Machine$integer.max))) 
      stop("Box constraints can only be used for 'L-BFGS-B' in 'optim'!")  
    or = optim(par=start, f=g, method=method, control=args)
  }
  
  e = getBestElement(opt.path, measureAggrNames(measures[[1]])[1])
  new("opt.result", learner, control, e$x, e$y, opt.path)
}
