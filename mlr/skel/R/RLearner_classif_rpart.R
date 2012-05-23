# FIXME: parms has to be in hyperparamter list

makeRLearner.classif.rpart = function() {
  makeRLearnerClassif(
    cl = "classif.rpart",
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
    twoclass = TRUE,
    multiclass = TRUE,
    missings = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}
		
trainLearner.classif.rpart = function(.learner, .task, .subset,  ...) {
  f = getFormula(.task)
  d = getTaskData(.task, .subset)
  # FIXME strange bug with envit,. beacuse we delete in formula
  if (.task$task.desc$has.weights) {
    rpart(as.formula(sprintf("%s~.", .task$task.desc$target)), data=d, weights=.task$weights[.subset], ...)
  } else {
    rpart(f, data=d, ...)
  }
}
	
predictLearner.classif.rpart = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="prob", "class")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}
