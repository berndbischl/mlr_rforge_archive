#' Initialize a new set of hyper params for a learner
#' Draw uniformly from region "lower to upper"
sampleHyperPars = function(par.set) {
  n = length(par.set@pars)
  z = numeric(n)
  for (i in seq_len(n)) {
    pd = par.set@pars[[i]]
    x = runif(1, pd@constraints$lower, pd@constraints$upper)
    if (pd@type == "integer")
      x = as.integer(round(x))
    z[i] = x
  }
  names(z) = names(par.set@pars)
  return(z)
}


#' Mutate 1 individual
#' - learner is changed stochastically 
#' - if learners was changed, new hyper params are initialized, otherwise hyper params mutation operator 
#' - bit are flipped independently  
mutate = function(x, learner.ns, par.sets, control) {
  # if we have more than 1 learner change it stochastically
  if (length(learner.ns) > 1 && runif(1) < control@prob.mut.learner) {
    x$learner = sample(setdiff(learner.ns, x$learner), 1)
    # learner changed, sample new params
    x$hyper.pars = sampleHyperPars(par.sets[[x$learner]])
  } else { 
    # learner not changed, mutate params
    x$hyper.pars = mutate.hp(x, par.sets, control)
  }
  # flips feature bits
  p = c(1-control@prob.mut.bit, control@prob.mut.bit)
  mut = sample(0:1, length(x$bits), replace=TRUE, p)
  x$bits = (x$bits + mut) %% 2
  return(x)
}  

#' Mutate hyper params with polynomial mutation
#' Integer params are rounded at the end. 
mutate.hp = function(x, par.sets, control) {
  ps = par.sets[[x$learner]]
  low = as.numeric(lower(ps))
  upp = as.numeric(upper(ps))
  hps = x$hyper.pars
  if (is.integer(hps))
    mode(hps) = "numeric"
  for (i in seq_along(hps)) {
    pm = pm_operator(control@mut.hp.eta, control@mut.hp.prob, lower=low[i], upper=upp[i])
    x$hyper.pars[i] = pm(hps[i])
    if (ps@pars[[i]]@type == "integer")
      x$hyper.pars[i] = as.integer(round(x$hyper.pars[i]))
  }
  return(x$hyper.pars)
}

#' Cross-over 2 individuals:
#' - if learners are different, 1 is sampled
#' - if learners are different, hyper params are taken from the sampled one, otherwise hyper params CX operator 
#' - bit are uniformly sampled 
crossover = function(x, y, par.sets, control) {
  z = list()
  z$learner = sample(c(x$learner, y$learner), 1)
  if (z$learner == x$learner && z$learner != y$learner)
    z$hyper.pars = x$hyper.pars
  else if (z$learner != x$learner && z$learner == y$learner)
    z$hyper.pars = y$hyper.pars
  else {
    z$hyper.pars = crossover.hps(x, y, par.sets, control)
  }
  if (any(is.na(z$hyper.pars)))
    stop(123)
  p = c(control@prob.cx, 1-control@prob.cx)
  z$bits = mapply(function(a,b) sample(c(a,b), 1, prob=p), x$bits, y$bits)
  return(z)
}

#' Cross-over 2 hyper-param vectors of the same structure with SBX
#' Integer params are  NOT rounded, as this is always done at the end of subsequent mutation! 
crossover.hps = function(x, y, par.sets, control) {
  ps = par.sets[[x$learner]]
  low = as.numeric(lower(ps))
  upp = as.numeric(upper(ps))
  ns = names(x$hyper.pars)
  n = length(x$hyper.pars)
  hps = numeric(n)
  for (i in seq_len(n)) {
    sbx = sbx_operator(control@mut.hp.eta, control@mut.hp.prob, lower=low[i], upper=upp[i])
    hps[i] = sample(sbx(matrix(as.numeric(c(x$hyper.pars[i], y$hyper.pars[i])), 1, 2)), 1)
  }
  names(hps) = ns
  return(hps)
}  

