#state: list(vars, rp) 

# get a single perf value for a state: first measure, aggregated by first aggr function
get.perf = function(state) {
	state$perf[1]
}

# get all aggr. perf values for a state: first measure, aggregated by first aggr function
get.perfs = function(state) {
	state$perf
}

make.path.el = function(es, accept=0) {
	list(vars = es$vars, perf = get.perfs(es), evals=es$evals, event=es$event, accept=accept)
}

make.es = function(vars, rp, measures, aggr, evals, event) {
	m = names(measures)
	a = names(aggr)	
	y = rp$agg[a, m]
	names(y) = sapply(names(aggr), function(a1) paste(a1, m, sep="."))	
	return(list(vars=vars, rp=rp, perf=y, evals=evals, event=event))
}

add.path = function(path, es, accept) {
	a = ifelse(accept, get(".mlr.vareval", envir=.GlobalEnv), -1)
	pe = make.path.el(es, accept = a)
	path[[length(path) + 1]] = pe
	return(path)
} 

add.path.els = function(path, ess, best) {
	for (i in 1:length(ess)) {
		es = ess[[i]]
		path = add.path(path, es, setequal(es$vars, best$vars))
	}
	return(path)
} 


# evals a single set of vars and return the corresponding state
eval.state <- function(learner, task, resampling, measures, aggr, vars, event) {
	rp = eval.rf(learner, task, resampling, measures, aggr, parset=NULL, ps.scale=NULL, ps.names=NULL, vars=vars)
	evals = get(".mlr.vareval", envir=.GlobalEnv)+1
	assign(".mlr.vareval", evals, envir=.GlobalEnv)
	make.es(vars=vars, rp=rp, measures=measures, aggr=aggr, evals=evals, event=event)
}

# evals a set of var-lists and return the corresponding states
eval.states = function(learner, task, resampling, measures, aggr, varsets, event) {
	rps = eval.varsets(learner, task, resampling, measures, aggr, varsets)
	evals = get(".mlr.vareval", envir=.GlobalEnv)
	evals2 = evals + length(varsets)
	assign(".mlr.vareval", evals2, envir=.GlobalEnv)
	f = function(x1,x2,x3,x4) make.es(x1, x2, measures=measures, aggr=aggr, x3, x4) 
	Map(f, varsets, rps, (evals+1):evals2, event)
}


# compare 2 states.  
# TRUE : state2 is significantly better than state1  
# compare = function(state1, state2, control, measures, aggr, threshold) 
	

# use the difference in performance   
compare.diff = function(state1, state2, control, measures, aggr, threshold) {
	m1 = get.perf(state1)
	m2 = get.perf(state2)
	d = ifelse(control$minimize, 1, -1) * (m1 - m2)
	(d > threshold)	
}



# select the best state from a list by using get.perf
select.best.state = function(states, control, measures, aggr) {
	perfs = lapply(states, get.perf)
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
