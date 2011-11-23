trafoVal = function(par, val) {
  if (is(par, "ParamSet"))
    Map(trafoVal, par$pars, val)
  else
    par@trafo(val)
}

# evals a set of var-lists and return the corresponding states

log.fun.tune = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  par.str = mlr:::valToString(par.set, x)
  logger.info(sprintf("[Tune] %i: %s : %s", length(opt.path), par.str, perfsToString(y)))
}

