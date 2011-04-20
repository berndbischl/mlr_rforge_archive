
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
#addPathElements = function(path, ess, best) {
#	for (i in 1:length(ess)) {
#		es = ess[[i]]
#		path = add.path(path, es, !is.null(best$par) && setequal(es$par, best$par))
#	}
#	return(path)
#} 

eval.states = function(learner, task, resampling, measures, par.set, bits.to.features, control, opt.path, pars, eol=NA, dob=NA) {
  y = mylapply(xs=pars, from="opt", f=mlr:::eval.rf, learner=learner, task=task, resampling=resampling, 
    measures=measures, par.set=par.set, bits.to.features=bits.to.features, control=control)
  n = length(pars)
  if (length(dob) == 1)
    dob = rep(dob, n)
  if (length(eol) == 1)
    eol = rep(eol, n)
  for (i in 1:n) 
    addPathElement(opt.path, x=pars[[i]], y=y[[i]], dob=dob[i], eol=eol[i])
  return(y)
}



# compare 2 states.  
# TRUE : state2 is significantly better than state1  
# compare = function(state1, state2, control, measures, threshold) 


# use the difference in performance   
compare.diff = function(state1, state2, control, measure, threshold) {
	ifelse(measure@minimize, 1, -1) * (state1$y[1] - state2$y[1]) > threshold
}

makeOptPathFromMeasures = function(x.names, measures) {
  minimize = Reduce(c, lapply(measures, function(m) rep(m@minimize, length(m@aggr))))
  makeOptPath(x.names, measuresAggrNames(measures), minimize)
}
