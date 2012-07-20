#' @S3method makeRLearner regr.lm
makeRLearner.regr.lm = function() {
  makeRLearnerRegr(
    cl = "regr.lm",
    package = "stats",
    par.set = makeParamSet(
  			 makeDiscreteLearnerParam(id="method", default="moment", values=c("moment", "mle", "mve", "t")),
  			 makeNumericLearnerParam(id="nu", lower=2, requires=expression(method=="t")),
      makeNumericLearnerParam(id="tol", default=1.0e-4, lower=0)
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = TRUE,
    weights = TRUE
  )
}
		
trainLearner.regr.lm = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  d = getTaskData(.task, .subset)
  if (.task$task.desc$has.weights) {
    # FIXME: strange bug in lm concerning weights
    .weights = force(.task$weights[.subset])
    xx <<- .weights
    lm(data=d, weights=.weights, ...)
  } else { 
    lm(f, data=d, ...)
  }
}
	
predictLearner.regr.lm = function(.learner, .model, .newdata, ...) {
  if(.learner$predict.type == "response") {
    predict(.model$learner.model, newdata=.newdata, se.fit=FALSE, ...)
  } else {
    p = predict(.model@learner.model, newdata=.newdata, se.fit=FALSE, ...)
    cbind(p$fit, p$se.fit)
  }
}
