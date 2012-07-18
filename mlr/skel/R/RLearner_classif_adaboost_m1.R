# FIXME: interface was changed, read page, pars, mnaybe rename
makeRLearner.classif.adaboost.m1 = function() {
  makeRLearnerClassif(
    cl = "classif.adaboost.m1",
    package = "adabag",
    par.set = makeParamSet( 
      makeLogicalLearnerParam(id="boos", default=TRUE),
      makeIntegerLearnerParam(id="mfinal", default=100L, lower=1L),
      makeDiscreteLearnerParam(id="coeflearn", default="Breiman", values=c("Breiman", "Freund")),
      # rpart.control arguments
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
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE
  )
}

trainLearner.classif.adaboost.m1 = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  xs = learnerArgsToControl(rpart.control, c("minsplit", "minbucket", "cp", "maxcompete", "maxsurrogate", "usesurrogate", "surrogatestyle", "maxdepth"), list(...))
  do.call(boosting, c(list(f, data=getTaskData(.task, .subset), control=xs$control), xs$args))
}

predictLearner.classif.adaboost.m1 = function(.learner, .model, .newdata, ...) {
  # stupid adaboost
  .newdata[, .model$task.desc$target] <- factor(rep(1, nrow(.newdata)), levels=.model$task.desc$class.levels)
  p = predict(.model$learner.model, newdata=.newdata, ...)
  return(as.factor(p$class))
}