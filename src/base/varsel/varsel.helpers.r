#state: list(vars, rp) 

# get a single perf value for a state: first measure, aggregated by first aggr function
get.perf = function(state, measures, aggr) {
	rp = state$rp
	m = names(measures)[1]
	a = names(aggr)[1]		
	rp$measures[a, m]
}

# evals a single set of vars and return the corresponding state
eval.state <- function(learner, task, resampling, measures, aggr, vars) {
	rp = eval.rf(learner, task, resampling, measures, aggr, parset=NULL, ps.scale=NULL, ps.names=NULL, vars=vars)
	assign(".mlr.vareval", get(".mlr.vareval", envir=.GlobalEnv)+1, envir=.GlobalEnv)
	return(list(vars=vars, rp=rp))
}

# evals a set of var-lists and return the corresponding states
eval.states = function(learner, task, resampling, measures, aggr, varsets) {
	rps = eval.varsets(learner, task, resampling, measures, aggr, varsets)
	assign(".mlr.vareval", get(".mlr.vareval", envir=.GlobalEnv)+length(varsets), envir=.GlobalEnv)
	mapply(function(a,b) list(vars=a, rp=b), varsets, rps, SIMPLIFY=FALSE)
}


# compare 2 states.  
# TRUE : state2 is significantly better than state1  
# compare = function(state1, state2, control, measures, aggr, forward) 
	

# use the difference in performance   
compare.diff = function(state1, state2, control, measures, aggr, forward) {
	m1 = get.perf(state1, measures, aggr)
	m2 = get.perf(state2, measures, aggr)
	d = ifelse(control$minimize, 1, -1) * (m1 - m2)
	thresh = ifelse(forward, control$alpha, control$beta)
	(d > thresh)	
}



# select the best state from a list by using get.perf
select.best.state = function(states, control, measures, aggr) {
	perfs = lapply(states, get.perf, measures=measures, aggr=aggr)
	if (control$minimize)
		i = which.min(perfs)
	else 
		i = which.max(perfs)
	return(states[[i]])
}
	
#select.best.diff = function(states, vals, control, forward) {
#	if (control$minimize)
#		i = which.max(vals)
#	else 
#		i = which.min(vals)
#	d = vals[[i]]
#	if (forward && d > control$alpha)
#		return(states[[i]])
#	if (!forward && d > control$beta)
#		return(states[[i]])
#	return(NULL)
#}
