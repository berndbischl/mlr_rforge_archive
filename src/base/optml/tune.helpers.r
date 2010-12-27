
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

