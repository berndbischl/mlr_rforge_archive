makeRLearner.regr.pcr = function() {
  makeRLearnerRegr(
    cl = "regr.pcr",
    package = "pls",
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.pcr = function(.learner, .task, .subset,  ...) {
  f = getFormula(.task)
  pcr(f, data=getTaskData(.task, .subset), ...)
}

predictLearner.regr.pcr = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata)
}