#todo: should we really define validation error like this?
tune.mbo = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  mbo.control = control@mbo.control
  # set final evals to 0 to save time. we dont really need final evals in this context.
  mbo.control@final.evals = 0L
  requirePackages(c("lhs"), "tune.mbo")
  f = makeTunerTargetFun(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
    arg.as.list=TRUE, trafo=FALSE)
  or = mbo(f, par.set, des=NULL, control@learner, mbo.control)
  new("OptResult", learner, control, or$x, or$y, opt.path)
}
