makeRLearner.classif.nb = function() {
  makeRLearnerClassif(
    cl = "classif.nb",
    package = "e1071",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="laplace", default=0, lower=0)
      # makeNumericLearnerParam(id="threshold", default=0.001, lower=0)
    ), 
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

trainLearner.classif.nb = function(.learner, .task, .subset,  ...) {
  f = getFormula(.task)
  naiveBayes(f, data=getData(.task, .subset), ...)
}

predictLearner.classif.nb = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type=="response", "class", "raw")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}


