
get.perf = function(state, measures, aggr) {
	rp = state$rp
	m = names(measures)[1]
	a = names(aggr)[1]		
	rp$measures[a, m]
}


eval.state <- function(learner, task, resampling, measures, aggr, vars) {
	rp = eval.rf(learner, task, resampling, measures, aggr, parset=NULL, ps.scale=NULL, ps.names=NULL, vars=vars) 
	return(list(vars=vars, rp=rp))
}

eval.states = function(learner, task, resampling, measures, aggr, varsets) {
	rps = eval.varsets(learner, task, resampling, measures, aggr, varsets)
	mapply(function(a,b) list(vars=a, rp=b), varsets, rps, SIMPLIFY=FALSE)
}



compare.diff = function(state1, state2, control, measures, aggr) {
	m1 = get.perf(state1, measures, aggr)
	m2 = get.perf(state2, measures, aggr)
	m1 - m2
}



s.best = function(states, control, measures, aggr) {
	perfs = lapply(states, get.perf, measures=measures, aggr=aggr)
	if (control$minimize)
		i = which.min(perfs)
	else 
		i = which.max(perfs)
	return(states[[i]])
}
	
select.best.diff = function(states, vals, control, forward) {
	if (control$minimize)
		i = which.max(vals)
	else 
		i = which.min(vals)
	d = vals[[i]]
	if (forward && d > control$alpha)
		return(states[[i]])
	if (!forward && d > control$beta)
		return(states[[i]])
	return(NULL)
}
