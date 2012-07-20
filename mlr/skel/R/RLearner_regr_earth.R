#' @S3method makeRLearner regr.earth
makeRLearner.regr.earth = function() {
  makeRLearnerRegr(
    cl = "regr.earth",
    package = "earth",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="degree", default=1, lower=1),
      makeNumericLearnerParam(id="penalty"),
      makeIntegerLearnerParam(id="nprune")
      ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.earth = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  earth(f, data=getTaskData(.task, .subset), ...)
}

predictLearner.regr.earth = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata)[,1]
}
