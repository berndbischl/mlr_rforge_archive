makeRLearner.regr.mars = function() {
  makeRLearnerRegr(
    cl = "regr.mars",
    package = "mda",
    missings = FALSE,
    numerics = TRUE,
    factors = FALSE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.mars = function(.learner, .task, .subset,  ...) {
  d = getTaskData(.task, .subset, target.extra=TRUE)
  mars(x = as.matrix(d$data), y = d$target, ...)
}

predictLearner.regr.mars = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata)
}