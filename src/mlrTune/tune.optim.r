# todo: add optimize if only 1 par
tune.optim = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  if (any(sapply(par.set@pars, function(x) !(x@type %in% c("numeric", "integer", "numericvector", "integervector")))))
    stop("Optim can only be applied to numeric, integer, numericvector, integervector parameters!")

  low = lower(par.set)
  upp = upper(par.set)
  if (length(control@start) != length(low))
    stop(" Length of 'start' has to match number of parameters in 'par.set'!")
  
  start = unlist(control@start)
  g = make.tune.f(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
    arg.as.list=FALSE, trafo=TRUE)
		
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

  i = getBestIndex(opt.path, measureAggrNames(measures[[1]])[1], ties="random")
  e = getPathElement(opt.path, i)
  new("OptResult", learner, control, e$x, e$y, opt.path)
}
