make.tune.f = function(learner, task, resampling, measure, par.set, control, opt.path) {
  function(p) {
    p2 = as.list(p)
    names(p2) = sapply(par.set@pars, function(p) p@id)
    
    #round integers
    for (i in 1:length(p2)) {
      if (par.set@pars[[i]]@type == "integer")
        p2[[i]] = as.integer(round(p2[[i]]))
    }
    # todo: what about operators that generate the new state? accepted?
    y = eval.rf(learner, task, resampling, measure, par.set, control, p2) 
    add.path.el(opt.path, x=p2, y=y)   
    ifelse(measures[[1]]["minimize"], 1 , -1) * perf
  }  
}

# evals a set of var-lists and return the corresponding states

tune.log.fun = function(learner, task, resampling, measure, par.set, control, opt.path) {
  logger.info(level="tune", paste(valToString(par.set, p2), " : ", formatC(perf, digits=3)))
}


