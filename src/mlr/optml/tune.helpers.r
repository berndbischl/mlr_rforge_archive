make.tune.f = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  function(p) {
    p2 = as.list(p)
    names(p2) = sapply(par.set@pars, function(p) p@id)
    
    #round integers
    for (i in 1:length(p2)) {
      if (par.set@pars[[i]]@type == "integer")
        p2[[i]] = as.integer(round(p2[[i]]))
    }
    
    p2 = trafoVal(par.set, p2)
    # todo: what about operators that generate the new state? accepted?
    y = eval.rf(learner, task, resampling, measures, par.set, control, p2) 
    addPathElement(opt.path, x=p2, y=y)   
    log.fun(learner, task, resampling, measures, par.set, control, opt.path, p2, y)
    ifelse(measures[[1]]@minimize, 1 , -1) * y[1]
  }  
}

# evals a set of var-lists and return the corresponding states

log.fun.tune = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  logger.info(level="opt", paste(valToString(par.set, x), " : ", formatC(y, digits=3)))
}


