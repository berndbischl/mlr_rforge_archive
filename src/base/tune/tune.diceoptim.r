tune.diceoptim = function(learner, task, resampling, measures, control) {
  require.packs(c("DiceOptim", "lhs"), "tune.diceoptim")
  
  
  args = control@extra.args
  print(args)
  des = init.design(control@par.descs, args$init.des.points)
  print(str(des))
  ns = colnames(des)
  low = control["lower"][ns]
  up = control["upper"][ns]

  penv = new.env()
  f = make.tune.f(ns, penv, learner, task, resampling, measures, control) 
  
  y = apply(des, 1, f) 
  
  m <- km(~1, design=des, response=y, covtype="gauss", control=list(trace=FALSE))
  s = capture.output(or <- EGO.nsteps(model=m, fun=f, nsteps=args$nsteps, lower=low, upper=up))
  
  j = which.min(or$value)
  
  par = as.list(or$par[j,])
  opt = get.path.el(penv$path, par)
  new("opt.result", control=control, opt=opt, path=penv$path)
}
