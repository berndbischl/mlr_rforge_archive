#' @S3method makeRLearner regr.lasso
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

#' @S3method trainLearner regr.lasso
trainLearner.regr.lasso = function(.learner, .task, .subset, .weights,  ...) {
  f = as.formula(getTaskFormulaAsString(.task))
  penalized(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner regr.lasso
predictLearner.regr.lasso = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  .newdata[,.model$task.desc$target] = 0
  penalized::predict(m, data=.newdata,  ...)[,"mu"]
}