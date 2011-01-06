tune.cmaes = function(learner, task, resampling, measures, control) {
  require.packs("cmaes", "tune.cmaes")

  penv = new.env()
  ns = control["par.names"]
  start = unlist(control["start"])[ns]
  low = control["lower"]
  up = control["upper"]
  
  g = make.tune.f(ns, penv, learner, task, resampling, measures, control)

  g2 = function(p) {
    p2 = as.list(as.data.frame(p))
    p2 = lapply(p2, function(x) {x=as.list(x);names(x)=ns;x})
    es = eval.states.tune(learner, task, resampling, measures, control, p2, "optim")
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
	par = as.list(or$par)
	names(par) = ns
	opt = get.path.el(penv$path, par)
	new("opt.result", control=control, opt=opt, path=penv$path)
}
