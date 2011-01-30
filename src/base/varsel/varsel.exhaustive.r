varsel.exhaustive = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  all.vars = task["input.names"]
  m = length(all.vars) 
  max.vars = control["max.vars"]
  
  states = list()
  for (i in 1:max.vars) {
    x = combn(1:m, i)
    states = c(states, lapply(1:ncol(x), function(j) all.vars[x[,j]]))
  }
  
  eval.states(learner, task, resampling, measures, control, states, "exh")
  e = getBestElement(opt.path, measureAggrNames(measures[[1]])[1])
  new("opt.result", learner, control, e$x, e$y, opt.path)
} 