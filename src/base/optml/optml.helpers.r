# todo: the global eval vars are very bad, think about parallel! 


#state: list(vars, rp) 

# get a single perf value for a state: first measure, aggregated by first aggr function
get.perf = function(state) {
	state$rp[1]
}

make.path.el = function(es, accept=0) {
	list(par = es$par, perf = es$rp, evals=es$evals, event=es$event, accept=accept)
}

make.es = function(par, rp, evals, event) {
	return(list(par=par, rp=rp, evals=evals, event=event))
}

#add.path = function(path, es, accept) {
#	a = ifelse(accept, {, -1)
#	pe = make.path.el(es, accept = a)
#	path[[length(path) + 1]] = pe
#	return(path)
#} 
#
## best = NULL means no acceptable new element was found
#add.path.els = function(path, ess, best) {
#	for (i in 1:length(ess)) {
#		es = ess[[i]]
#		path = add.path(path, es, !is.null(best$par) && setequal(es$par, best$par))
#	}
#	return(path)
#} 

eval.states = function(learner, task, resampling, measures, bounds, control, opt.path, pars) {
  y = mylapply(xs=pars, from="opt", f=eval.rf, learner=learner, task=task, resampling=resampling, 
    measures=measures, bounds=bounds, control=control)
  
  for (i in 1:length(pars))
    add.path.el(opt.path, pars[[i]], y[i])
  return(y)
}



# compare 2 states.  
# TRUE : state2 is significantly better than state1  
# compare = function(state1, state2, control, measures, threshold) 


# use the difference in performance   
compare.diff = function(state1, state2, control, measure, threshold) {
	m1 = get.perf(state1)
	m2 = get.perf(state2)
	d = ifelse(measure["minimize"], 1, -1) * (m1 - m2)
  (d > threshold)	
}



# select the best state from a list by using get.perf
select.best.state = function(states, measure) {
	perfs = sapply(states, get.perf)
	if (measure["minimize"])
		i = which.min(perfs)
	else 
		i = which.max(perfs)
  # all perfs can be NA if all learners failed, then select randomly
  if (all(is.na(perfs))) {
    warning("All evaluated states had NA performance, selecting 1 randomly!")
    i = sample(1:length(perfs), 1)
  }
	return(states[[i]])
}

# retrieve el from path
get.path.el = function(path, par) {
	Find(function(x) identical(x$par, par), path)
} 


