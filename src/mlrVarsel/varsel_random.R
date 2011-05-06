varsel.random = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, log.fun) {
  prob = control@prob
  states = lapply(1:control@maxit, function(i) rbinom(length(bit.names), 1, prob))
  eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, states)
  e = getBestElement(opt.path, measureAggrNames(measures[[1]])[1])
  new("OptResult", learner, control, bits.to.features(e$x, task), e$y, opt.path)
} 