getParamSet.BaseWrapper = function(learner) {
  c(learner$par.set, getParamSet(learner$learner))
} 

#' @S3method getHyperPars BaseWrapper
getHyperPars.BaseWrapper = function(learner, for.fun) {
  print(for.fun)
  c(getHyperPars(learner$learner, for.fun), getHyperParsTop(learner, for.fun))
}

#' @S3method setHyperPars2 BaseWrapper
setHyperPars2.BaseWrapper = function(learner, par.vals) {
  ns = names(par.vals)
  pds.n = names(learner$par.set$pars)
  for (i in seq(length=length(par.vals))) {
    if (ns[i] %in% pds.n) {
      learner = callNextMethod(learner, par.vals=par.vals[i])
    } else {	
      learner$learner = setHyperPars(learner$learner, par.vals=par.vals[i])
    }
  }
  return(learner)
}


getLeafLearner = function(learner) {
  if (is(learner, "BaseWrapper"))
    return(getLeafLearner(learner$learner))
  else 
    return(learner)
}



getHyperParsTop = function(learner, for.fun) {
  wh = switch(for.fun, 
              train=c("train", "both"), 
              predict=c("predict", "both"),
              both=c("train", "predict", "both")
  )
  pv = learner$par.vals
  ns = names(Filter(function(y) y$when %in% wh, learner$par.set$pars))
  ns = intersect(ns, names(learner$par.vals))
  pv[ns]
}
