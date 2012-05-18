makeRLearner.regr.kknn = function() {
  makeRLearnerRegr(
    cl = "regr.kknn",
    package = "kknn",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="k", default=7L, lower=1L),
      makeNumericLearnerParam(id="distance", default=2, lower=0),
      makeDiscreteLearnerParam(id="kernel", default="triangular", 
        values=list("rectangular", "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian"))
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.kknn = function(.learner, .task, .subset,  ...) {
  list(td=.task$desc, data=getTaskData(.task, .subset), parset=list(...))
}

predictLearner.regr.kknn = function(.learner, .model, .newdata, ...) {
  m <- .model$learner.model
  f = getFormula(.model$task.desc)
  # this is stupid but kknn forces it....
  .newdata[, m$td$target] <- 0
  pars <- list(formula=f, train=m$data, test=.newdata)  
  pars <- c(pars, m$parset, list(...))
  m <- do.call(kknn, pars)
  return(m$fitted.values)
}