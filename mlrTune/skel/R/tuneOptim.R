# todo: add optimize if only 1 par
tuneOptim = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  low = getLower(par.set)
  upp = getUpper(par.set)
  
  start = unlist(control@start)
  g = makeTunerTargetFun(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
    arg.as.list=FALSE, trafo=TRUE)
		
	args = control@extra.args
	method = args$method
	if(is.null(method)) 
		method = "Nelder-Mead"
	args$method = NULL
  
  if (method == "L-BFGS-B") {
    or = optim(par=start, f=g, method=method, lower=low, upper=upp, control=args)
  } else {
    # todo: fix machine bound
    if (any((is.double(low) & low != -Inf) | (is.integer(low) & low != -.Machine$integer.max)) ||
        any((is.double(upp) & upp !=  Inf) | (is.integer(upp) & upp !=  .Machine$integer.max))) 
      stop("Box constraints can only be used for 'L-BFGS-B' in 'optim'!")  
    or = optim(par=start, f=g, method=method, control=args)
  }
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  new("OptResult", learner, control, e$x, e$y, opt.path)
}
