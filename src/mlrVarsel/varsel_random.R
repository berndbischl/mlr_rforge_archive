varsel.random = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, log.fun) {
  prob = control@prob
  states = lapply(1:control@maxit, function(i) rbinom(length(bit.names), 1, prob))
  mlrTune:::eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, states)
  i = getOptPathBestIndex(opt.path, measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
  new("OptResult", learner, control, bits.to.features(e$x, task), e$y, opt.path)
} 