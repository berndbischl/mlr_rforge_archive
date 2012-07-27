#FIXME: should we really define validation error like this?
tuneMBO = function(learner, task, resampling, measures, par.set, control,
  opt.path, show.info, log.fun) {

  requirePackages(c("mlrMBO"), "tuneMBO")
  mbo.control = control$mbo.control
  # set final evals to 0 to save time. we dont really need final evals in this context.
  mbo.control$final.evals = 0L
  or = mbo(tunerFitnFun, par.set, des=NULL, control$learner, mbo.control)
  makeOptResult(learner, control, or$x, or$y, opt.path)
}
