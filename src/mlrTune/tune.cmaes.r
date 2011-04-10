# set initial varaince to (upper-lower)/2 if both bounds are given
#todo: fix parallel g2
tune.cmaes = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  require.packs("cmaes", "tune.cmaes")

  if (any(sapply(par.set@pars, function(x) !(x@type %in% c("numeric", "integer", "numericvector", "integervector")))))
    stop("CMAES can only be applied to numeric, integer, numericvector, integervector parameters!")
  
  low = lower(par.set)
  upp = upper(par.set)
  if (length(control@start) != length(low))
    stop(" Length of 'start' has to match number of parameters in 'par.set'!")
  
  start = unlist(control@start)
  g = make.tune.f(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
    arg.as.list=FALSE, trafo=TRUE)

#  g2 = function(p) {
#    p2 = as.list(as.data.frame(p))
#    p2 = lapply(p2, function(x) {x=as.list(x);names(x)=ns;x})
#    es = eval.states(learner, task, resampling, measures, control, p2, "optim")
#    path <<- addPathElements.tune(path=path, ess=es, best=NULL)
#    perf = sapply(es, get.perf)
#    # cma es does not like NAs which might be produced if the learner gets values which result in a degenerated model
#    if (measures[[1]]@minimize)
#      perf[is.na(perf)] = Inf
#    else
#      perf[is.na(perf)] = -Inf
#    return(perf)
#  }
  
	args = control@extra.args
	
  if (.mlr.local$parallel.setup$mode != "local" && .mlr.local$parallel.setup$level == "tune") {
    g=g2
    args$vectorized=TRUE    
  }  
  or = cma_es(par=start, fn=g, lower=low, upper=upp, control=args)
	e = getBestElement(opt.path, measureAggrNames(measures[[1]])[1])
	new("OptResult", learner, control, e$x, e$y, opt.path)
}
