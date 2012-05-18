makeRLearner.regr.ksvm = function() {
  makeRLearnerRegr(
    cl = "regr.ksvm",
    package = "kernlab",
    par.set = makeParamSet(
      makeLogicalLearnerParam(id="scaled", default=TRUE),
      makeDiscreteLearnerParam(id="type", default="eps-svr", values=c("eps-svr", "nu-svr", "eps-bsvr")),
      makeDiscreteLearnerParam(id="kernel", default="rbfdot", 
        values=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
      makeNumericLearnerParam(id="C",
        lower=0, default=1, requires=expression(type %in% c("eps-svr", "eps-bsvr"))),
      makeNumericLearnerParam(id="nu",
        lower=0, default=0.2, requires=expression(type == "nu-svr")),
      makeNumericLearnerParam(id="epsilon", lower=0, default=0.1),
      makeNumericLearnerParam(id="sigma",
        lower=0, requires=expression(kernel %in% c("rbfdot", "anovadot", "besseldot", "laplacedot"))),
      makeIntegerLearnerParam(id="degree", default=3L, lower=1L, 
        requires=expression(kernel %in% c("polydot", "anovadot", "besseldot"))),
      makeNumericLearnerParam(id="scale", default=1, lower=0, 
        requires=expression(kernel %in% c("polydot", "tanhdot"))),
      makeNumericLearnerParam(id="offset", default=1, 
        requires=expression(kernel %in% c("polydot", "tanhdot"))),
      makeIntegerLearnerParam(id="order", default=1L, 
        requires=expression(kernel == "besseldot")),
      makeNumericLearnerParam(id="tol", default=0.001, lower=0),
      makeLogicalLearnerParam(id="shrinking", default=TRUE)
    ), 
    missings = FALSE,
    numerics = TRUE,
    factors = TRUE,
    se = FALSE,
    weights = FALSE
  )
}

trainLearner.regr.ksvm = function(.learner, .task, .subset,  ...) {
  xs = learnerArgsToControl(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda"), list(...))
  f = getFormula(.task)
  # difference in missing(kpar) and kpar=list()!
  if (length(xs$control) > 0)
    args = c(list(f, data=getTaskData(.task, .subset), fit=FALSE, kpar=xs$control), xs$args)
  else
    args = c(list(f, data=getTaskData(.task, .subset), fit=FALSE), xs$args)
  do.call(ksvm, args)
}

predictLearner.regr.ksvm = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata=.newdata, ...)[,1]
}