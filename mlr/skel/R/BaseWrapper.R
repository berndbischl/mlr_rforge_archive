makeBaseWrapper = function(learner, pack=character(0), par.set, par.vals=list()) {
  if (is(learner, "OptWrapper")) 
    stop("Cannot wrap an optimization wrapper with something else!")
  ns = intersect(names(par.set$pars), names(learner@par.set$pars))
  if (length(ns) > 0)
    stop("Hyperparameter names in wrapper clash with base learner names: ", paste(ns, collapse=","))
	.Object@learner = learner
  .Object@pack = c(pack, learner@pack) 
  .Object = callNextMethod(.Object, par.set=par.set, par.vals=par.vals, pack=pack)
  .Object@properties = learner@properties 
  return(.Object)
}
  
trainLearner.BaseWrapper = function(.learner, .task, .subset,  ...) {
  trainLearner(.learner@learner, .task, .subset, ...)
}

predictLearner.BaseWrapper = function(.learner, .model, .newdata, ...) {
  predictLearner(.learner@learner, .model, .newdata, ...)
}

getParamSet.BaseWrapper = function(learner) {
  c(learner@par.set, getParamSet(learner@learner))
} 

getHyperPars.BaseWrapper = function(learner, for.fun) {
  c(getHyperPars(learner@learner, for.fun), getHyperParsTop(learner, for.fun))
}

setHyperPars2.BaseWrapper = function(learner, par.vals) {
  ns = names(par.vals)
  pds.n = names(learner@par.set$pars)
  for (i in seq(length=length(par.vals))) {
  	if (ns[i] %in% pds.n) {
		  learner = callNextMethod(learner, par.vals=par.vals[i])
		} else {	
			learner@learner = setHyperPars(learner@learner, par.vals=par.vals[i])
		}
  }
  return(learner)
} 
 
print.BaseWrapper = function(x, ...) {
  s = ""
  y = object 
  while (is(y, "BaseWrapper")) {
    s = paste(s, class(y), "->", sep="")
    y = y@learner
  }
  s = paste(s, class(y))
  
  cat(
    s, "\n",
    "Hyperparameters: ", getHyperParsString(object), "\n\n",
    sep = ""         
  )
}




