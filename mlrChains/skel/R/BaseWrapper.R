# FIXME: test this
makeBaseWrapper = function(learner, pack=character(0), par.set, par.vals=list()) {
  if (inherits(learner, "OptWrapper")) 
    stop("Cannot wrap an optimization wrapper with something else!")
  ns = intersect(names(par.set$pars), names(learner$par.set$pars))
  if (length(ns) > 0)
    stopf("Hyperparameter names in wrapper clash with base learner names: %s", collapse(ns))
  
  structure(list(
    id = id,
    type = type,
    package = c(pack, learner$package),
    par.set = par.set,
    par.vals = list(),
    numerics = learner$numerics,
    factors = learner$factors,
    predict.type = "response",
    missings = learner$missings,
    weights = learner$weights,
    oneclass = learner$oneclass,
    twoclass = learner$twoclass,
    multiclass = learner$multiclass,
    prob = learner$prob,
    se = learner$se,
    learner = learner    
  ), class = c("BaseWrapper", "Learner"))
}

  
trainLearner.BaseWrapper = function(.learner, .task, .subset,  ...) {
  trainLearner(.learner$learner, .task, .subset, ...)
}

predictLearner.BaseWrapper = function(.learner, .model, .newdata, ...) {
  predictLearner(.learner$learner, .model, .newdata, ...)
}

getParamSet.BaseWrapper = function(learner) {
  c(learner$par.set, getParamSet(learner$learner))
} 

#' @S3method getHyperPars BaseWrapper
getHyperPars.BaseWrapper = function(learner, for.fun) {
  print(for.fun)
  c(getHyperPars(learner$learner, for.fun), getHyperParsTop(learner, for.fun))
}

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
# FIXME: test
#' @S3method print BaseWrapper
print.BaseWrapper = function(x, ...) {
  s = ""
  y = x
  while (inherits(y, "BaseWrapper")) {
    s = paste(s, class(y), "->", sep="")
    y = y$learner
  }
  s = paste(s, class(y)[1])
  
  cat(
    s, "\n",
    "  Hyperparameters: ", getHyperParsString(x), "\n\n",
    sep = ""         
  )
}
