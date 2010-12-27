# approach by forrester for noisy functions:
#  fit kriging with nugget effect
#  predict design points
#  now interpolate these without nugget, use estimated hyperpars from 1st model (proven to be optimal in his paper IIRC) 
km2 = function(design, response, covtype="matern5_2", optim.method = "BFGS", lower=NULL, upper=NULL) {
  m = km(design=design, response=response, covtype=covtype, optim.method=optim.method, lower=lower, upper=upper,
    nugget.estim=TRUE)
  y = predict(m, design, type="SK")
  m = km(design=design, response=y, covtype=covtype, optim.method=optim.method, lower=lower, upper=upper,
    nugget.estim=FALSE, coef.trend = m@coef.trend, coef.cov = m@coef.cov, coef.var=m@coef.var)
}
  
myCL.nsteps = function(model, fun, control) {
  n <- nrow(model@X)

  # fit initial model
  model = km2()
  
  
  g2 = function(p) {
    p2 = as.list(as.data.frame(p))
    p2 = lapply(p2, function(x) {x=as.list(x);names(x)=ns;x})
    es = eval.states.tune(learner, task, resampling, measures, control, p2, "optim")
    path <<- add.path.els.tune(path=path, ess=es, best=NULL)
    perf = sapply(es, get.perf)
    # cma es does not like NAs which might be produced if the learner gets values which result in a degenerated model
    if (measures[[1]]["minimize"])
      perf[is.na(perf)] = Inf
    else
      perf[is.na(perf)] = -Inf
    return(perf)
  }
  
  for (i in 1:control["seq.loops"]) {
    L <- min(model@y)
    res.CL <- max_qEI.CL(model, npoints = npoints, L = L, 
      lower = lower, upper = upper, parinit = parinit, 
      control = control)
    model@X <- rbind(model@X, res.CL$par)
    
    y.real = g2(t(res.CL$par))
    model@y = rbind(model@y, y.real)
    
    model = km2(design = model@X, response = model@y, covtype = model@covariance@name, 
      lower = model@lower, upper = model@upper, optim.method = control["optim.method"])
  }
  
#  return(list(par = model@X[(n + 1):(n + nsteps * npoints), 
#        , drop = FALSE], value = model@y[(n + 1):(n + nsteps * 
#            npoints), , drop = FALSE], npoints = npoints, nsteps = nsteps, 
#      lastmodel = model))
}
