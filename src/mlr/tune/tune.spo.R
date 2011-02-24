tune.spo = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  spo.control = control@spo.control
  require.packs(c("lhs"), "tune.spo")
  f = make.tune.f(learner, task, resampling, measures, par.set, control, opt.path, log.fun, arg.as.list=TRUE)
  r = spo(f, par.set, des=NULL, control@learner, spo.control)
  e = getBestElement(opt.path)
  new("opt.result", learner, control, e$x, e$y, opt.path)
}
