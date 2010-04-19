
eval.parsets = function(learner, task, resampling, measures, aggr, pars, ps.scale, ps.names) {
	rps = mylapply(xs=pars, from="tune", f=eval.rf, 
			learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, ps.scale=ps.scale, ps.names=ps.names, vars=NULL)
	return(rps)
}

# evals a set of var-lists and return the corresponding states
eval.states.tune = function(learner, task, resampling, measures, aggr, pars, event, ps.scale, ps.names) {
	eval.states(".mlr.tuneeval", eval.parsets, learner, task, resampling, measures, aggr, pars, event, ps.scale, ps.names)
}


add.path.els.tune = function(path, ess, best) {
	add.path.els(".mlr.tuneeval", path, ess, best)	
} 

