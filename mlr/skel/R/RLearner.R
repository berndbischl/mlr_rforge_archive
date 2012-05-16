makeRLearner = function(type, package, par.set, numerics, factors, missings, weights) {
  structure(list(
    package = package,
    par.set = par.set,
    numerics = numerics,
    factors = factors,
    missings = missings,
    weights = weights,
  ), class = "RLearner")
}

#' Wraps an already implemented learning method from R to make it accessible to mlr.
makeRLearnerClassif = function(type, package, par.set, numerics=FALSE, factors=FALSE, 
  missings=FALSE, weights=FALSE, oneclass=FALSE, twoclass=FALSE, multiclass=FALSE, prob=FALSE) {
  x = makeRLearner(type, package, par.set, numerics, factors, missings, weights)
  x$oneclass = oneclass
  x$twoclass = twoclass
  x$multiclass = multiclass
  x$prob = prob
  class(x) = c("RLearnerClassif", class(x))  
  return(x)
}


makeRLearnerRegr = function(type, package, par.set, numerics, factors=FALSE,
  missings=FALSE, weights=FALSE, se=FALSE) {
  x = makeRLearner(type, package, par.set, numerics, factors, missings, weights)
  x$se = se
  class(x) = c("RLearnerRegr", class(x))  
  return(x)
}

print.RLearner = function(x, ...) {
  pack = paste(object@pack, collapse=",")
  cat(
    "Learner ", object@id, "' from package ", pack, "\n",
    "Type: ", object@type, "\n",
    "Class: ", class(object)[1], "\n",
    "Predict-Type: ", object@predict.type, "\n",
    "Hyperparameters: ", getHyperParsString(object), "\n\n",
    "Supported features Doubles:", x$numerics, " Factors:", x$factors, "\n",
    "Supports missings: ", x$missings, "\n", 
    "Supports weights: ", x$weights, "\n", 
    "Supports standard errs: ", x$se, "\n", 
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


