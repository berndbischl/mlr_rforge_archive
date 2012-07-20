#' @S3method makeRLearner classif.glmboost
makeRLearner.classif.glmboost = function() {
  makeRLearnerClassif(
    cl = "classif.glmboost",
    package = "mboost",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id="family", default=Binomial(), values=list(AdaExp=AdaExp(), Binomial=Binomial())),
      makeIntegerLearnerParam(id="mstop", default=100L, lower=1L),
      makeNumericLearnerParam(id="nu", default=0.1, lower=0, upper=1),        
      makeLogicalLearnerParam(id="center", default=FALSE)
    ), 
    par.vals = list(family=Binomial()),
    oneclass = FALSE,
    twoclass = TRUE,
    multiclass = FALSE,
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

trainLearner.classif.glmboost = function(.learner, .task, .subset, mstop, nu, risk, ...) {
  ctrl = learnerArgsToControl(boost_control, mstop, nu, risk)
  f = getTaskFormula(.task)
  if (.task$task.desc$has.weights)
    glmboost(f, data=getTaskData(.task, .subset), control=ctrl, weights=.task$weights[.subset], ...)
  else
    glmboost(f, data=getTaskData(.task, .subset), control=ctrl, , ...)
}

predictLearner.classif.glmboost = function(.learner, .model, .newdata, ...) {
  type = ifelse(.learner$predict.type == "response", "class", "response")
  p = predict(.model$learner.model, newdata=.newdata, type=type, ...)
  if (.learner$predict.type  == "prob") {
    p = p[,1]
    y = matrix(0, ncol=2, nrow=nrow(.newdata))
    colnames(y) <- .model$task.desc$class.levels
    y[,1] = p
    y[,2] = 1-p
    return(y)
  } else {
    return(p)
  }
}