varsel.random = function(learner, task, resampling, measures, par.set, control, opt.path, log.fun) {
  all.vars = getFeatureNames(task)
	m = length(all.vars) 
	prob = control["prob"]
	
	states = list()
	for (i in 1:control["maxit"]) {
		vs = all.vars[as.logical(rbinom(m, 1, prob))]
		states[[i]] = vs
	}
	
	es = eval.states(learner, task, resampling, measures, control, states, "random")
	bs = select.best.state(es, measures)
	
    
  new("opt.result", control=control, opt=make.path.el(state), path=path)
}	