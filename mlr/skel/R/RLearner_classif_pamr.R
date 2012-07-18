makeRLearner.classif.pamr = function() {
  makeRLearnerClassif(
    cl = "classif.pamr",
    package = "pamr",
    par.set = makeParamSet(
      makeNumericVectorLearnerParam(id="threshold"),
      makeIntegerLearnerParam(id="n.threshold", lower=1L, default=30L),
      makeLogicalLearnerParam(id="scale.sd", default=TRUE),
      makeNumericVectorLearnerParam(id="threshold.scale", lower=0),
      makeNumericVectorLearnerParam(id="se.scale", default=5L, lower=0),
      makeDiscreteLearnerParam(id="offset.percent", values=0:2),
      makeUntypedLearnerParam(id="hetero", default=NULL),
      makeNumericVectorLearnerParam(id="prior", lower=0, upper=1),
      makeLogicalLearnerParam(id="remove.zeros", default=TRUE),
      makeUntypedLearnerParam(id="sign.contrast", default="both"),
      makeNumericVectorLearnerParam(id="threshold", when="predict")
    ), 
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    prob = TRUE
  )
}

trainLearner.classif.pamr = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  data = getTaskData(.task, .subset, target.extra=TRUE)
  names(data)[1] = "x"  
  names(data)[2] = "y"
  data$x = t(data$x)
  pamr.train(data, ...)
}

predictLearner.classif.pamr = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="posterior", "class")
  pamr.predict(.model$learner.model, newx=t(.newdata), type=type, threshold=1, ...)
}