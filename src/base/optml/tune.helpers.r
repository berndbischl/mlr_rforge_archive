
eval.parsets = function(learner, task, resampling, measures, control, pars) {
	rps = mylapply(xs=pars, from="tune", f=eval.rf, learner=learner, task=task, resampling=resampling, 
			measures=measures, control=control)
	return(rps)
}

# evals a set of var-lists and return the corresponding states
eval.states.tune = function(learner, task, resampling, measures, control, pars, event) {
	eval.states(".mlr.tuneeval", eval.fun=eval.parsets, 
			learner=learner, task=task, resampling=resampling, 
			measures=measures, control=control, pars=pars, event=event)
}

eval.state.tune = function(learner, task, resampling, measures, control, par, event) {
	eval.state(".mlr.tuneeval", learner, task, resampling, 
			measures=measures, control=control, par=par, event=event)
}

add.path.tune = function(path, es, accept) {
	add.path(".mlr.tuneeval", path, es, accept)
} 

add.path.els.tune = function(path, ess, best) {
	add.path.els(".mlr.tuneeval", path, ess, best)	
} 

make.pds.from.lowup = function(ns, lower, upper) {
  pds = list()
  if (length(lower) == 1 && is.null(names(lower))) {
    lower = rep(lower, length(start))
    names(lower) = ns
  }
  if (length(upper) == 1 && is.null(names(upper))) {
    upper = rep(upper, length(start))
    names(upper) = ns
  }
  if (!setequal(ns, names(lower)))
    stop("Names of argument lower should be:", paste(ns, collapse=", "))
  if (!setequal(ns, names(upper)))
    stop("Names of argument upper should be:", paste(ns, collapse=", "))
  for (i in 1:length(ns)) {
    p = ns[i]
    pd = new("par.desc.double", par.name=p, lower=lower[p], upper=upper[p])
    pds[[i]] = pd 
  }
  return(pds)
}

