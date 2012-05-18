makeRLearner.classif.kknn = function() {
  makeRLearnerClassif(
    cl = "classif.kknn",
    package = "kknn",
    #todo: find out what ykernel and contrasts really do 
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="k", default=7L, lower=1L),
      makeNumericLearnerParam(id="distance", default=2, lower=0),
      makeDiscreteLearnerParam(id="kernel", default="triangular", 
        values=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))
    ), 
    oneclass = FALSE,
    twoclass = TRUE,
    multiclass = TRUE,
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE,
    weights = FALSE
  )
}

trainLearner.classif.kknn = function(.learner, .task, .subset,  ...) {
  list(td=.task$task.desc, data=getTaskData(.task, .subset), parset=list(...))
}

predictLearner.classif.kknn = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  f = getFormula(.model$task.desc)
  # this is stupid but kknn forces it....
  .newdata[, m$td$target] <- 0
  pars <- list(formula=f, train=m$data, test=.newdata)  
  pars <- c(pars, m$parset, list(...))
  m <- do.call(kknn, pars)
  if (.learner$predict.type == "response")
    return(m$fitted.values)
  else 
    return(m$prob)
}
 