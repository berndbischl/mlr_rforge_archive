#todo: should we really define validation error like this?
tune.spo = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  spo.control = control@spo.control
  # set final evals to 0 to save time. we dont really need final evals in this context.
  spo.control@final.evals = 0L
  requirePackages(c("lhs"), "tune.spo")
  f = makeTunerTargetFun(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
    arg.as.list=TRUE, trafo=FALSE)
  or = spo(f, par.set, des=NULL, control@learner, spo.control)
  new("OptResult", learner, control, or$x, or$y, opt.path)
}
