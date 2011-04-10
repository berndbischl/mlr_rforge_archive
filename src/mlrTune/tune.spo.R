#todo: should we really define validation error like this?
tune.spo = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  spo.control = control@spo.control
  require.packs(c("lhs"), "tune.spo")
  f = make.tune.f(learner, task, resampling, measures, par.set, control, opt.path, log.fun, 
    arg.as.list=TRUE, trafo=FALSE)
  or = spo(f, par.set, des=NULL, control@learner, spo.control)
  df = as.data.frame(opt.path)
  inds = (nrow(df)-control@spo.control@final.evals+1):nrow(df)
  y = colMeans(df[inds, opt.path@y.names])
  new("OptResult", learner, control, or$x, y, opt.path)
}
