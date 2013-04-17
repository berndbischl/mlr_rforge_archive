tuneRandom = function(learner, task, resampling, measures, par.set, control, opt.path, show.info, log.fun) {
  vals = sampleValues(n=control$extra.args$maxit, par=par.set)
  evalOptimizationStates(learner, task, resampling, measures, par.set, NULL, control, opt.path, 
    show.info, logFunTune, vals, dobs=1L, eols=1L)
  i = getOptPathBestIndex(opt.path, mlr:::measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  makeTuneResult(learner, control, e$x, e$y, opt.path)
}


