# todo: the global eval vars are very bad, think about parallel! 


#state: list(vars, rp) 

# get a single perf value for a state: first measure, aggregated by first aggr function

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

eval.states = function(learner, task, resampling, measures, par.set, control, opt.path, pars) {
  y = mylapply(xs=pars, from="opt", f=eval.rf, learner=learner, task=task, resampling=resampling, 
    measures=measures, par.set=par.set, control=control)
  
  for (i in 1:length(pars))
    add.path.el(opt.path, x=pars[[i]], y=y[[i]])
  
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

makeOptimizationPathFromMeasures = function(par.set, measures) {
  minimize = Reduce(c, lapply(measures, function(m) rep(m@minimize, length(m@aggr))))
  makeOptimizationPath(par.set, measuresAggrNames(measures), minimize)
}