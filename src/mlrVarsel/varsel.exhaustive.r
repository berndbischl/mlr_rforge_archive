varsel.exhaustive = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, log.fun) {
  p = length(bit.names)
  states = list(rep(0, p))
  for (i in 1:min(control["max.vars"], p)) {
    x = combn(1:p, i)
    s = lapply(1:ncol(x), function(j) { 
        b = rep(0, p)
        b[x[,j]] = 1
        b
    })
    states = c(states, s)
  }
  eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, states)
  e = getBestElement(opt.path, measureAggrNames(measures[[1]])[1])
  new("opt.result", learner, control, bits.to.features(e$x, task), e$y, opt.path)
} 