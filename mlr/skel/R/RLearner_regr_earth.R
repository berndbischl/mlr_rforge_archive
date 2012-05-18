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
  #x = getTaskData(.task, .subset, target.extra=TRUE)
  #earth(x$data, x$target, ...)
  f = getFormula(.task)
  args = list(f, data=getTaskData(.task, .subset))
  args = c(args, list(...))
  do.call("earth", args)
}

predictLearner.regr.earth = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata)[,1]
}
