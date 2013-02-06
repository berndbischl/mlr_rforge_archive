#' @S3method makeRLearner regr.ridge
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

#' @S3method trainLearner regr.ridge
trainLearner.regr.ridge = function(.learner, .task, .subset, .weights,  ...) {
  f = as.formula(getTaskFormulaAsString(.task))
  penalized(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner regr.ridge
predictLearner.regr.ridge = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data=.newdata,  ...)[,"mu"]
}