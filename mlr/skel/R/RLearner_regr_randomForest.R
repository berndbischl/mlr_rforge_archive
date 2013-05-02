#' @S3method makeRLearner regr.randomForest
makeRLearner.regr.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="ntree", default=500L, lower=1L),
      makeIntegerLearnerParam(id="mtry", lower=1L),
      makeLogicalLearnerParam(id="replace", default=TRUE),
      makeIntegerLearnerParam(id="sampsize", lower=1L),
      makeIntegerLearnerParam(id="nodesize", default=1L, lower=1L),
      makeIntegerLearnerParam(id="maxnodes", lower=1L),
      makeLogicalLearnerParam(id="importance", default=FALSE),
      makeLogicalLearnerParam(id="localImp", default=FALSE),
      makeLogicalLearnerParam(id="keep.inbag", default=FALSE)
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

#' @S3method trainLearner regr.randomForest
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights,  ...) {
  f = getTaskFormula(.task)
  randomForest(f, data=getTaskData(.task, .subset), ...)
}

#' @S3method predictLearner regr.randomForest
predictLearner.regr.randomForest = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata, ...)
}