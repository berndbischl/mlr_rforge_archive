# FIXME use max.features
selectFeaturesRandom = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {
  prob = control$prob
  states = lapply(1:control$maxit, function(i) rbinom(length(bit.names), 1, prob))
  evalOptimizationStates(learner, task, resampling, measures, bits.to.features, control, opt.path, show.info, states, 1L, as.integer(NA))
  i = getOptPathBestIndex(opt.path, mlr:::measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  makeFeatSelResult(learner, control, e$x, e$y, opt.path)
} 