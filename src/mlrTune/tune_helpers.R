eval.states = function(learner, task, resampling, measures, par.set, bits.to.features, control, opt.path, pars, 
  eol=as.integer(NA), dob=as.integer(NA)) {
  
  y = mylapply(xs=pars, from="opt", f=eval.rf, learner=learner, task=task, resampling=resampling, 
    measures=measures, par.set=par.set, bits.to.features=bits.to.features, control=control)
  n = length(pars)
  if (length(dob) == 1)
    dob = rep(dob, n)
  if (length(eol) == 1)
    eol = rep(eol, n)
  for (i in 1:n) 
    addPathElement(opt.path, x=as.list(pars[[i]]), y=y[[i]], dob=dob[i], eol=eol[i])
  return(y)
}


trafoVal = function(par, val) {
  if (is(par, "ParameterSet"))
    Map(trafoVal, par@pars, val)
  else
    par@trafo(val)
}


make.tune.f = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun, arg.as.list=TRUE, trafo=TRUE) {
  function(p) {
    pars = par.set@pars
    if (arg.as.list) {
      p.split = p
    } else {
      ids = getRepeatedParameterIDs(par.set, with.nr=FALSE)
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
    if (trafo)
      p.split2 = trafoVal(par.set, p.split)
    else
      p.split2 = p.split
    # todo: what about operators that generate the new state? accepted?
    
    learner = setHyperPars(learner, par.vals=p.split2)
    y = resample(learner, task, resampling, measures=measures)$aggr
    addPathElement(opt.path, x=p.split, x.trafo=p.split2, y=y)   
    
    log.fun(learner, task, resampling, measures, par.set, control, opt.path, p.split2, y)
    ifelse(measures[[1]]@minimize, 1 , -1) * y[1]
  }  
}

# evals a set of var-lists and return the corresponding states

log.fun.tune = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  mlr:::logger.info(level="opt", paste(valToString(par.set, x), " : ", 
      paste(formatC(y, digits=3), collapse=",")))
}

