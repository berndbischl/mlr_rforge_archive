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
  y.name = measureAggrNames(measures[[1]])[1]
  e = getBestElement(opt.path, y.name, dobs=1)
  
  new("opt.result", control=control, par=e$x, y=e$y, path=path)
} 