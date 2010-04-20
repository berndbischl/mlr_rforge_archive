#state: list(vars, rp) 

# get a single perf value for a state: first measure, aggregated by first aggr function
get.perf = function(state) {
	state$rp$aggr[1,1]
}

# get all aggr. perf values for a state in a vector
flat.perfs = function(state) {
	mm = state$rp$aggr
	# todo: bad code, must be shorter!
	mm2 = reshape(mm, ids=row.names(mm), times=names(mm), varying=list(names(mm)), direction="long")
	if(length(measures)==1)
		rownames(mm2) = paste(rownames(mm), colnames(mm), sep=".")
	mm2 = t(mm2[,2, drop=FALSE])
	ns =  colnames(mm2)
	mm2 = as.numeric(mm2)
	names(mm2) = ns
	return(mm2)	
}

make.path.el = function(es, accept=0) {
	list(par = es$par, perf = flat.perfs(es), evals=es$evals, event=es$event, accept=accept)
}

make.es = function(par, rp, evals, event) {
	return(list(par=par, rp=rp, evals=evals, event=event))
}

add.path = function(global.eval.var, path, es, accept) {
	a = ifelse(accept, get(global.eval.var, envir=.GlobalEnv), -1)
	pe = make.path.el(es, accept = a)
	path[[length(path) + 1]] = pe
	return(path)
} 

add.path.els = function(global.eval.var, path, ess, best) {
	for (i in 1:length(ess)) {
		es = ess[[i]]
		path = add.path(global.eval.var, path, es, setequal(es$par, best$par))
	}
	return(path)
} 


eval.state = function(type, global.eval.var, learner, task, resampling, measures, aggr, par, event, ...) {
	if (type == "varsel")
		rp = eval.rf(learner, task, resampling, measures, aggr, 
				parset=NULL, ps.scale=NULL, ps.names=NULL, vars=par)
	else
		rp = eval.rf(learner, task, resampling, measures, aggr, 
				vars=NULL, parset=par, ...)
	evals = get(global.eval.var, envir=.GlobalEnv)+1
	assign(global.eval.var, evals, envir=.GlobalEnv)
	make.es(par=par, rp=rp, evals=evals, event=event)
}

# evals a set of var-lists and return the corresponding states
eval.states = function(global.eval.var, eval.fun, learner, task, resampling, measures, aggr, pars, event, ...) {
	rps = eval.fun(learner, task, resampling, measures, aggr, pars, ...)
	evals = get(global.eval.var, envir=.GlobalEnv)
	evals2 = evals + length(pars)
	assign(global.eval.var, evals2, envir=.GlobalEnv)
	f = function(x1,x2,x3,x4) make.es(x1, x2, x3, x4) 
	Map(f, pars, rps, (evals+1):evals2, event)
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
select.best.state = function(states, control) {
	perfs = sapply(states, get.perf)
	if (control$minimize)
		i = which.min(perfs)
	else 
		i = which.max(perfs)
	return(states[[i]])
}

