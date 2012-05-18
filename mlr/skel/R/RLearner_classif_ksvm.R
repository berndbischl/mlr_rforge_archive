makeRLearner.classif.ksvm = function() {
  makeRLearnerClassif(
    cl = "classif.ksvm",
    package = "kernlab",
    # TODO: stringdot pars and check order, scale and offset limits
    par.set = makeParamSet(
      makeLogicalLearnerParam(id="scaled", default=TRUE),
      makeDiscreteLearnerParam(id="type", default="C-svc", values=c("C-svc", "nu-svc", "C-bsvc", "spoc-svc", "kbb-svc")),
      makeDiscreteLearnerParam(id="kernel", default="rbfdot", 
        values=c("vanilladot", "polydot", "rbfdot", "tanhdot", "laplacedot", "besseldot", "anovadot", "splinedot", "stringdot")),
      makeNumericLearnerParam(id="C",
        lower=0, default=1, requires=expression(type %in% c("C-svc", "C-bsvc", "spoc-svc", "kbb-svc"))),
      makeNumericLearnerParam(id="nu",
        lower=0, default=0.2, requires=expression(type == "nu-svc")),
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
      makeLogicalLearnerParam(id="shrinking", default=TRUE),
      makeNumericLearnerParam(id="class.weights", default=1, lower=0)
    ), 
    twoclass = TRUE,
    multiclass = TRUE,
    numerics = TRUE,
    factors = TRUE,
    prob = TRUE
  )
}

trainLearner.classif.ksvm = function(.learner, .task, .subset,  ...) {

  # TODO custom kernel. freezes? check mailing list
  # TODO unify cla + regr, test all sigma stuff
  
#     # there's a strange behaviour in r semantics here wgich forces this, see do.call and the comment about substitute
#     if (!is.null(args$kernel) && is.function(args$kernel) && !is(args$kernel,"kernel")) {
#       args$kernel = do.call(args$kernel, kpar)  
#     } 
  
  xs = learnerArgsToControl(list, c("degree", "offset", "scale", "sigma", "order", "length", "lambda", "normalized"), list(...))
  f = getFormula(.task)
  pm = .learner$predict.type == "prob"
  if (length(xs$control) > 0)
    args = c(list(f, data=getData(.task, .subset), fit=FALSE, kpar=xs$control), xs$args, prob.model=pm)
  else
    args = c(list(f, data=getData(.task, .subset), fit=FALSE), xs$args, prob.model=pm)
  do.call(ksvm, args)
  
}

predictLearner.classif.ksvm = function(.learner, .model, .newdata, ...) {
  type = switch(.learner$predict.type, prob="probabilities", "response")
  predict(.model$learner.model, newdata=.newdata, type=type, ...)
}