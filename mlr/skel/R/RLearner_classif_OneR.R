makeRLearner.classif.OneR = function() {
  makeRLearnerClassif(
    cl = "classif.OneR",
    package = "RWeka",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="reltoll", default=1.0e-8)      
    ), 
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

trainLearner.classif.OneR = function(.learner, .task, .subset,  ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}

predictLearner.classif.OneR = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
