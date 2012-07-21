makeOptPathDFFromMeasures = function(par.set, measures) {
  ns = sapply(measures, mlr:::measureAggrName)
  if (any(duplicated(ns)))
    stop("Cannot create OptPath, measures do not have unique ids!")
  if (length(intersect(ns, names(par.set$pars))) > 0 ||
    length(intersect(ns, getParamIds(par.set, repeated=TRUE, with.nr=TRUE))) > 0)
    stop("Cannot create OptPath, measures ids and dimension names of input space overlap!")
  minimize = sapply(measures, function(m) m$minimize)
  makeOptPathDF(par.set, ns, minimize)
}


# evals a set of var-lists and return the corresponding states
logFunTune = function(learner, task, resampling, measures, par.set, control, opt.path, x, y) {
  par.str = paramValueToString(par.set, x)
  messagef("[Tune] %i: %s : %s", getOptPathLength(opt.path), par.str, mlr:::perfsToString(y))
}

