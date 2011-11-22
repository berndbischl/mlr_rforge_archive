tune.diceoptim = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  requirePackages(c("DiceOptim", "lhs"), "tune.diceoptim")
  
  if (any(sapply(par.set@pars, function(x) !(x@type %in% c("numeric", "integer")))))
    stop("DiceOptim can only be applied to numeric and integer parameters!")
  
  low = lower(par.set)
  up = upper(par.set)
  if (any(is.infinite(c(low, up))))
    stop("DiceOptim requires finite box constraints!")
  
  args = control@extra.args
  des = makeDesign(args$init.des.points, par.set)
  ns = colnames(des)

  f = make.tune.f(learner, task, resampling, measures, par.set, control, opt.path) 
  
  y = apply(des, 1, f) 
  
  m <- km(~1, design=des, response=y, covtype="gauss", control=list(trace=FALSE))
  s = capture.output(or <- EGO.nsteps(model=m, fun=f, nsteps=args$nsteps, lower=low, upper=up))
  
  j = which.min(or$value)
  
  par = as.list(or$par[j,])
  i = getBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getPathElement(opt.path, i)
  
  new("OptResult", learner, control, e$x, e$y, opt.path)
}
