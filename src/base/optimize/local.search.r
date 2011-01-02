
generate.offspring = function(learner, task, res, par.vals, y, par.descs, control) {
  new.vals = list()
  for (j in 1:control["lambda"]) {
    i = sample(1:length(par.descs), 1)
    pd = par.descs[[i]]
    s = sample(c(-1L,1L), 1)
    pn = par.desc["par.name"]
    pv = par.vals[[pn]]
    # todo check requires of pars
    if (is(pd, "par.desc.num")) {
      step.size = step.sizes[pn]
      new.val = pv + s*step.size  
    } else if (is(pd, "par.desc.log")) {
      new.val = !pv  
    } else if (is(pd, "par.desc.disc")) {
      vals = pd["vals"]
      new.val = vals[[sample(1:length(vals), 1)]]    
    } else {
      stop("Unkown par.desc!")
    }
    new.vals[[j]] = new.val
    names(new.vals)[j] = pn
  }
}


local.search = function(learner, task, res, par.vals, y, par.descs, control) {
  pd = par.descs[[i]]
  s = sample(c(-1L,1L), 1)
  pn = par.desc["par.name"]
  pv = par.vals[[pn]]
  offs = generate.offspring(earner, task, res, par.vals, y, par.descs, control)
  new.vals[[pn]] = new.val
  p = resample(learner, task, res, par.vals=new.vals)
  perf = performance(p, task, measures=measures, aggr=aggr)
  
  if (perf < y)
    Recall(learner, task, res, par.vals, y, par.descs, step.sizes)  
  else  
    Recall(learner, task, res, par.vals, y, par.descs, step.sizes)  
}
