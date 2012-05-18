# checked props

makeRLearner.classif.j48 = function() {
  makeRLearnerClassif(
    cl = "classif.j48",
    package = "Rweka",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id="U"),
      makeLogicalLearnerParam(id="O"),
      makeNumericLearnerParam(id="C", default=0.25, lower=0),
      makeIntegerLearnerParam(id="M", default=2L, lower=1L),
      makeLogicalLearnerParam(id="R"),
      makeIntegerLearnerParam(id="N", default=3L, lower=2L),
      makeLogicalLearnerParam(id="B"),
      makeLogicalLearnerParam(id="S"),
      makeLogicalLearnerParam(id="L"),
      makeLogicalLearnerParam(id="A"),
      makeLogicalLearnerParam(id="J")
    ), 
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

trainLearner.classif.j48 = function(.learner, .task, .subset,  ...) {
  f = getFormula(.task)
  ctrl = Weka_control(..., Q=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
  J48(f, data=getTaskData(.task, .subset), control=ctrl)
}

predictLearner.classif.j48 = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
