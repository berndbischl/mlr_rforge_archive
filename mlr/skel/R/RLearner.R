makeRLearner = function(id, type, package, par.set, numerics, factors, missings, weights, 
  oneclass, twoclass, multiclass, prob, se, par.vals) {

  checkArg(par.set, "ParamSet")  
  requirePackages(package, paste("learner", .Object$id))
  checkListElementClass(par.set$pars, "LearnerParam")

  learner = structure(list(
    id = id,
    type = type,
    package = package,
    par.set = par.set,
    numerics = numerics,
    factors = factors,
    predict.type = "response",
    missings = missings,
    weights = weights,
    oneclass = oneclass,
    twoclass = twoclass,
    multiclass = multiclass,
    prob = prob,
    se = se
  ), class = c("RLearner", "Learner"))
  setHyperPars(learner, par.vals=par.vals)
}

#' Wraps an already implemented learning method from R to make it accessible to mlr.
makeRLearnerClassif = function(cl, package, par.set, numerics=FALSE, factors=FALSE, 
  missings=FALSE, weights=FALSE, oneclass=FALSE, twoclass=FALSE, multiclass=FALSE, 
  prob=FALSE, par.vals=list()) {
  
  x = makeRLearner(cl, "classif", package, par.set, numerics, factors, missings, weights, 
    oneclass, twoclass, multiclass, prob, FALSE, par.vals)
  class(x) = c(cl, "RLearnerClassif", class(x))  
  return(x)
}


makeRLearnerRegr = function(cl, package, par.set, numerics, factors=FALSE,
  missings=FALSE, weights=FALSE, se=FALSE, par.vals=list()) {
  
  x = makeRLearner(cl, "regr", package, par.set, numerics, factors, missings, weights,
    FALSE, FALSE, FALSE, FALSE, se, par.vals)
  class(x) = c(cl, "RLearnerRegr", class(x))  
  return(x)
}

print.RLearner = function(x, ...) {
  cat(
    "Learner ", x$id, " from package ", collapse(x$package), "\n",
    "Type: ", x$type, "\n",
    "Class: ", class(x)[1], "\n",
    "Predict-Type: ", x$predict.type, "\n",
    "Hyperparameters: ", getHyperParsString(x), "\n\n",
    "Supported features Numerics:", x$numerics, " Factors:", x$factors, "\n",
    "Supports missings: ", x$missings, "\n", 
    "Supports weights: ", x$weights, "\n", 
    sep =""
  )
}

print.RLearnerClassif = function(x, ...) {
  print.RLearner(x)
  catf("Supports classes: %s", 
    collapse(c("one", "two", "multi")[c(x$oneclass, x$twoclass, x$multiclass)]))
  catf("Supports probabilities: %s", x$prob) 
}

print.RLearnerRegr = function(x, ...) {
  print.RLearner(x)
  catf("Supports standard errs: %s", x$se) 
}


