make.tune.f = function(learner, task, resampling, measure, par.set, control, opt.path) {
  function(p) {
    p2 = as.list(p)
    names(p2) = sapply(par.set@pars, function(p) p@id)
    # todo: what about operators that generate the new state? accepted?
    y = eval.rf(learner, task, resampling, measure, par.set, control, p2) 
    add.path.el(opt.path, x=p2, y=y)   
    perf = get.perf(es)
    logger.info(level="tune", paste(ns, "=", formatC(p, digits=3)), ":", formatC(perf, digits=3))
    ifelse(measures[[1]]["minimize"], 1 , -1) * perf
  }  
}

# evals a set of var-lists and return the corresponding states




