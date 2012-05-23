makeRLearner.regr.lasso = function() {
  makeRLearnerRegr(
    cl = "regr.lasso",
    package = "penalized",
    par.set = makeParamSet(
      makeNumericLearnerParam(id="lambda1", default=0, lower=0)
    ), 
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.lasso = function(.learner, .task, .subset,  ...) {
  f = getFormula(.task)
  penalized(f, data=getTaskData(.task, .subset), ...)
}

predictLearner.regr.lasso = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  predict(m, data=.newdata,  ...)[,"mu"]
}