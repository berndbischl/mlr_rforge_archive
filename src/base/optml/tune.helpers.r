
add.path.tune = function(path, es, accept) {
	add.path(path, es, accept)
} 

add.path.els.tune = function(path, ess, best) {
	add.path.els(path, ess, best)	
} 

make.tune.f = function(ns, penv, learner, task, resampling, measures, control) {
  function(p) {
    p2 = as.list(p)
    names(p2) = ns
    es = eval.state.tune(learner, task, resampling, measures, control, p2, "optim")
    penv$path = add.path.tune(penv$path, es, accept=TRUE)   
    perf = get.perf(es)
    logger.info(level="tune", paste(ns, "=", formatC(p, digits=3)), ":", formatC(perf, digits=3))
    ifelse(measures[[1]]["minimize"], 1 , -1) * perf
  }  
}

