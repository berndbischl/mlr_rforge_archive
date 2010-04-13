
varsel.random = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	all.vars = task["input.names"]
	m = length(all.vars) 
	
	states = list()
	for (i in 1:control$maxit) {
		vs = all.vars[as.logical(rbinom(m, 1, 0.5))]
		states[[i]] = vs
	}
	
	es = eval.states(learner, task, resampling, measures, aggr, states, "random")
	bs = select.best.state(es, control, measures, aggr)
	
	path = add.path.els(list(), es, bs)	
	list(opt=make.path.el(bs), path = path) 
}	