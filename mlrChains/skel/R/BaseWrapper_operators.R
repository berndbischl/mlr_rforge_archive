#' @S3method getParamSet BaseWrapper
getParamSet.BaseWrapper = function(learner) {
  c(learner$par.set, getParamSet(learner$learner))
} 

#' @S3method getHyperPars BaseWrapper
getHyperPars.BaseWrapper = function(learner, for.fun="train") {
  x = mlr:::getHyperPars.Learner(learner, for.fun)
  c(getHyperPars(learner$learner, for.fun), mlr:::getHyperPars.Learner(learner, for.fun))
}

#' @S3method setHyperPars2 BaseWrapper
setHyperPars2.BaseWrapper = function(learner, par.vals) {
  ns = names(par.vals)
  pds.n = names(learner$par.set$pars)
  for (i in seq(length=length(par.vals))) {
    if (ns[i] %in% pds.n) {
      learner = mlr:::setHyperPars2.Learner(learner, par.vals=par.vals[i])
    } else {	
      learner$learner = mlr:::setHyperPars2(learner$learner, par.vals=par.vals[i])
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



