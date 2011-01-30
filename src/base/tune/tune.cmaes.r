tune.cmaes = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  require.packs("cmaes", "tune.cmaes")

  if (any(sapply(par.set@pars, function(x) !(x@type %in% c("numeric", "integer")))))
    stop("CMAES can only be applied to numeric and integer parameters!")
  ns = sapply(par.set@pars, function(x) x@id)
  if (length(control@start) != length(ns))
    stop(" Length of 'start' has to match numer of parameters in 'par.set'!")
  low = lower(par.set)
  up = upper(par.set)
  
  start = unlist(control["start"])[ns]
  
  g = make.tune.f(learner, task, resampling, measures, par.set, control, opt.path)

  g2 = function(p) {
    p2 = as.list(as.data.frame(p))
    p2 = lapply(p2, function(x) {x=as.list(x);names(x)=ns;x})
    es = eval.states(learner, task, resampling, measures, control, p2, "optim")
    path <<- add.path.els.tune(path=path, ess=es, best=NULL)
    perf = sapply(es, get.perf)
    # cma es does not like NAs which might be produced if the learner gets values which result in a degenerated model
    if (measures[[1]]["minimize"])
      perf[is.na(perf)] = Inf
    else
      perf[is.na(perf)] = -Inf
    return(perf)
  }
  
	args = control@extra.args
	
  if (.mlr.local$parallel.setup$mode != "local" && .mlr.local$parallel.setup$level == "tune") {
    g=g2
    args$vectorized=TRUE    
  }  
  or = cma_es(par=start, fn=g, lower=low, upper=up, control=args)
	e = getBestElement(op.path, measureAggrNames(measures[[1]])[1])
	new("opt.result", learner, control, e$x, e$y, opt.path)
}
