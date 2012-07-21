# set initial varaince to (upper-lower)/2 if both bounds are given
#FIXME: fix parallel g2
tuneCMAES = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  requirePackages("cmaes", "tune_cmaes")
  
  low = getLower(par.set)
  upp = getUpper(par.set)

  start = unlist(control@start)
  g = makeTunerTargetFun(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
    arg.as.list=FALSE, trafo=TRUE)

#  g2 = function(p) {
#    p2 = as.list(as.data.frame(p))
#    p2 = lapply(p2, function(x) {x=as.list(x);names(x)=ns;x})
#    es = eval.states(learner, task, resampling, measures, control, p2, "optim")
#    path <<- addOptPathEls.tune(path=path, ess=es, best=NULL)
#    perf = sapply(es, get.perf)
#    # cma es does not like NAs which might be produced if the learner gets values which result in a degenerated model
#    if (measures[[1]]@minimize)
#      perf[is.na(perf)] = Inf
#    else
#      perf[is.na(perf)] = -Inf
#    return(perf)
#  }
  
	args = control$extra.args
	
#  if (.mlr.conf$parallel.setup$mode != "local" && .mlr.conf$parallel.setup$level == "tune") {
#    g=g2
#    args$vectorized=TRUE    
#  }  
  or = cma_es(par=start, fn=g, lower=low, upper=upp, control=args)
  i = getOptPathBestIndex(opt.path, mlr:::measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
	makeOptResult(learner, control, e$x, e$y, opt.path)
}
