
varsel.exhaustive = function(learner, task, resampling, measures, control) {
  all.vars = task["input.names"]
  m = length(all.vars) 
  max.vars = control["max.vars"]
  
  states = list()
  for (i in 1:max.vars) {
    x = combn(1:m, i)
    states = c(states, lapply(1:ncol(x), function(j) all.vars[x[,j]]))
  }
  
  es = eval.states(learner, task, resampling, measures, control, states, "exh")
  bs = select.best.state(es, measures[[1]])
  
  path = add.path.els.varsel(list(), es, bs)
  new("opt.result", control=control, opt=make.path.el(bs), path=path)
} 