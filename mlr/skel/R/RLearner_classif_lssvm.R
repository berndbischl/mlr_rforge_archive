makeRLearner.classif.lssvm = function() {
  makeRLearnerClassif(
    cl = "classif.lssvm",
    package = "kernlab",
    # to do: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeLogicalLearnerParam(id="scaled", default=TRUE),
      makeDiscreteLearnerParam(id="kernel", default="rbfdot", 
        values=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
      makeNumericLearnerParam(id="tau", lower=0, default=0.01),
      makeLogicalLearnerParam(id="reduced", default=TRUE),
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
      makeNumericLearnerParam(id="tol", default=0.0001, lower=0)
    ),
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = TRUE
  )
}

trainLearner.classif.lssvm = function(.learner, .task, .subset,  ...) {
# TODO custom kernel. freezes? check mailing list
# TODO unify cla + regr, test all sigma stuff  

#     # there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
#     if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
#       args$kernel = do.call(args$kernel, kpar)  
#     } 
  
  xs = learnerArgsToControl(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda", "normalized"), list(...))
  f = getFormula(.task)
  if (length(xs$control) > 0)
    args = c(list(f, data=getData(.task, .subset), fit=FALSE, kpar=xs$control), xs$args)
  else
    args = c(list(f, data=getData(.task, .subset), fit=FALSE), xs$args)
  do.call(lssvm, args)
  
}

predictLearner.classif.lssvm = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, "response")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}