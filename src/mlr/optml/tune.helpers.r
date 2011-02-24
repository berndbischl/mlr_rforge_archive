make.tune.f = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun, arg.as.list=TRUE) {
  function(p) {
    print(p)
    pars = par.set@pars
    if (arg.as.list) {
      p.split = p
    } else {
      ids = getRepeatedParameterIDs(par.set)
      p.split = split(p, ids)
    }
    p.split = Map(function(par, x) { 
      if (par@type %in% c("integer", "integervector"))
        as.integer(round(x))
      else
        x
      }, 
      pars, p.split
    )
    p.split = trafoVal(par.set, p.split)
    # todo: what about operators that generate the new state? accepted?
    y = eval.rf(learner, task, resampling, measures, par.set, NULL, control, p.split)
    print(y)
    addPathElement(opt.path, x=p.split, y=y)   
    log.fun(learner, task, resampling, measures, par.set, control, opt.path, p.split, y)
    ifelse(measures[[1]]@minimize, 1 , -1) * y[1]
  }  
}

# evals a set of var-lists and return the corresponding states

log.fun.tune = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  logger.info(level="opt", paste(valToString(par.set, x), " : ", formatC(y, digits=3)))
}

splitVector = function(x, par.set) {
  
}


