tune.diceoptim = function(learner, task, resampling, measure, bounds, control, logger) {
  require.packs(c("DiceOptim", "lhs"), "tune.diceoptim")
  
  if (any(sapply(bounds@pars, function(x) !(x@type %in% c("numeric", "integer")))))
    stop("DiceOptim can only be applied to numeric and integer parameters!")
  
  low = lower(bounds)
  up = upper(bounds)
  if (any(is.infinite(c(low, up))))
    stop("DiceOptim requires finite box constraints!")
  
  args = control@extra.args
  print(args)
  des = init.design(control@par.descs, args$init.des.points)
  print(str(des))
  ns = colnames(des)

  penv = new.env()
  f = make.tune.f(ns, penv, learner, task, resampling, measure, control, logger) 
  
  y = apply(des, 1, f) 
  
  m <- km(~1, design=des, response=y, covtype="gauss", control=list(trace=FALSE))
  s = capture.output(or <- EGO.nsteps(model=m, fun=f, nsteps=args$nsteps, lower=low, upper=up))
  
  j = which.min(or$value)
  
  par = as.list(or$par[j,])
  opt = get.path.el(penv$path, par)
  new("opt.result", control=control, opt=opt, path=penv$path)
}
