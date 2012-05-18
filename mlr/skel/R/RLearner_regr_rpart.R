makeRLearner.regr.rpart = function() {
  makeRLearnerRegr(
    cl = "regr.rpart",
    package = "rpart",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="minsplit", default=20L, lower=1L),
      makeIntegerLearnerParam(id="minbucket", lower=1L),
      makeNumericLearnerParam(id="cp", default=0.01, lower=0, upper=1),
      makeIntegerLearnerParam(id="maxcompete", default=4L, lower=0L),
      makeIntegerLearnerParam(id="maxsurrogate", default=5L, lower=0L),
      makeDiscreteLearnerParam(id="usesurrogate", default=2L, values=0:2),
      makeDiscreteLearnerParam(id="surrogatestyle", default=0L, values=0:1),
      # we use 30 as upper limit, see docs of rpart.control
      makeIntegerLearnerParam(id="maxdepth", default=30L, lower=1L, upper=30L)
    ), 
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = TRUE
  )
}

trainLearner.regr.rpart = function(.learner, .task, .subset,  ...) {
  f = getFormula(.task)
  if (.task@desc@has.weights)
    rpart(f, data=getData(.task, .subset), weights=.task@weights[.subset], ...)
  else  
    rpart(f, data=getData(.task, .subset), ...)
}

predictLearner.regr.rpart = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata, ...)
}