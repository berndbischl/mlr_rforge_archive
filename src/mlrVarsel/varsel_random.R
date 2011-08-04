varsel.random = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, log.fun) {
  prob = control@prob
  states = lapply(1:control@maxit, function(i) rbinom(length(bit.names), 1, prob))
  eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, states)
  i = getBestIndex(opt.path, measures[[1]]@id, ties="random")
  e = getPathElement(opt.path, i)
  new("OptResult", learner, control, bits.to.features(e$x, task), e$y, opt.path)
} 