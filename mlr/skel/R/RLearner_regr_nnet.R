makeRLearner.regr.nnet = function() {
  makeRLearnerRegr(
    cl = "regr.nnet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="size", default=3L, lower=0, pass.default=TRUE),
      makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
      makeNumericLearnerParam(id="decay", default=0, lower=0)
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

trainLearner.regr.nnet = function(.learner, .task, .subset,  ...) {
  f = getFormula(.task)
  if (.task$desc$has.weights)
    nnet(f, data=getTaskData(.task, .subset), linout=T, weights=.task$weights[.subset], ...)
  else  
    nnet(f, data=getTaskData(.task, .subset), linout=T, ...)
}

predictLearner.regr.nnet = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata, ...)[,1]
}