eval.varsets = function(learner, task, resampling, measures, aggr, varsets) {
	rps = mylapply(xs=varsets, from="varsel", f=eval.rf, 
			learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, parset=NULL, ps.scale=NULL, ps.names=NULL)
	return(rps)
}

# evals a set of var-lists and return the corresponding states
eval.states.varsel = function(learner, task, resampling, measures, aggr, pars, event) {
	eval.states(".mlr.vareval", eval.varsets, learner, task, resampling, measures, aggr, pars, event)
}


eval.state.varsel = function(learner, task, resampling, measures, aggr, par, event) {
	eval.state("varsel", ".mlr.vareval", learner, task, resampling, measures, aggr, par, event)
}


add.path.varsel = function(path, es, accept) {
	add.path(".mlr.vareval", path, es, accept)
} 

add.path.els.varsel = function(path, ess, best) {
	add.path.els(".mlr.vareval", path, ess, best)	
} 
