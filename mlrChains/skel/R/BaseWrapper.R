# FIXME: test this
makeBaseWrapper = function(learner, package=character(0), par.set=makeParamSet(), par.vals=list()) {
  checkArg(learner, "Learner")
  checkArg(package, "character", na.ok=FALSE)
  checkArg(par.set, "ParamSet")
  checkArg(par.vals, "list")
  if (!isProperlyNamed(par.vals))
    stop("'par.vals' must be a properly named list!")
  
  if (inherits(learner, "OptWrapper")) 
    stop("Cannot wrap an optimization wrapper with something else!")
  ns = intersect(names(par.set$pars), names(learner$par.set$pars))
  if (length(ns) > 0)
    stopf("Hyperparameter names in wrapper clash with base learner names: %s", collapse(ns))
  
  structure(list(
    id = learner$id,
    type = learner$type,
    package = c(package, learner$package),
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
