# evals a set of var-lists and return the corresponding states

log.fun.tune = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  par.str = paramValueToString(par.set, x)
  logger.info(sprintf("[Tune] %i: %s : %s", getOptPathLength(opt.path), par.str, perfsToString(y)))
}

