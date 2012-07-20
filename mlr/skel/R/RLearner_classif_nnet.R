#' @S3method makeRLearner classif.nnet
makeRLearner.classif.nnet = function() {
  makeRLearnerClassif(
    cl = "classif.nnet",
    package = "nnet",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="size", default=3L, lower=0L),
      makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
      # nnet seems to set these manually and hard for classification.....
#     makeLogicalLearnerParam(id="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#     makeLogicalLearnerParam(id="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#     makeLogicalLearnerParam(id="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#     makeLogicalLearnerParam(id="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
      makeLogicalLearnerParam(id="skip", default=FALSE),
      makeNumericLearnerParam(id="rang", default=0.7),
      makeNumericLearnerParam(id="decay", default=0),
      makeLogicalLearnerParam(id="Hess", default=FALSE),
      makeLogicalLearnerParam(id="trace", default=TRUE),
      makeIntegerLearnerParam(id="MaxNWts", default=1000L),
      makeNumericLearnerParam(id="abstoll", default=1.0e-4),
      makeNumericLearnerParam(id="reltoll", default=1.0e-8)      
    ), 
    par.vals = list(size=3L),
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = TRUE
  )
}

trainLearner.classif.nnet = function(.learner, .task, .subset,  ...) {
  f = getTaskFormula(.task)
  if (.task$task.desc$has.weights)
    nnet(f, data=getTaskData(.task, .subset), weights=.task$weights[.subset], ...)
  else  
    nnet(f, data=getTaskData(.task, .subset), ...)      
}

predictLearner.classif.nnet = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, response="class", prob="raw")
  p = predict(.model$learner.model, newdata=.newdata, type=type, ...)
  if (type == "class")
    return(as.factor(p))
  else {
    if (length(.model$task.desc$class.levels) == 2) {
      y <- cbind(p, 1-p) 
      colnames(y) = .model$task.desc$class.levels
      return(y)
    } else
      return(p) 
  }
}
