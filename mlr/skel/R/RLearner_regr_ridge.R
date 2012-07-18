makeRLearner.regr.ridge = function() {
  makeRLearnerRegr(
    cl = "regr.ridge",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="lambda2", default=0, lower=0)
    ), 
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.ridge = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  penalized(f, data=getTaskData(.task, .subset), ...)
}

predictLearner.regr.ridge = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  predict(m, data=.newdata,  ...)[,"mu"]
}