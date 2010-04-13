
varsel.bestcor = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	all.vars = task["input.names"]
	path = list()
	
	start.vars = character(0)	
	state = eval.state(learner, task, resampling, measures, aggr, vars=start.vars, event="start")
	
	path = add.path(path, state, T)		
	data = task["data"]
	not.used = all.vars
	cors = abs(cor(data[, all.vars], data[, task["target"]])) 
	o = order(cors, decreasing=T)
	not.used = not.used[o]
	#print(cors)
	while (TRUE) {
		#print("current:")
		#print(state$vars)
		found = FALSE
		for (i in seq(along.with=not.used)) {
			#print(.mlr.vareval)
			v = not.used[i]
			#cat(v, "\n")
			new.vars = c(state$vars, v)
			if (.mlr.vareval >= control$maxit)
				break
			s = eval.state(learner, task, resampling, measures, aggr, vars=new.vars, "forward")
			cc = compare.diff(state, s, control, measures, aggr, control$alpha)
			path = add.path(path, s, accept=cc)		
			if (cc) {
				state = s
				not.used = not.used[-i]
				found = TRUE
				break
			}
		}
		if (!found)
			break
	}
	list(opt=make.path.el(state), path = path) 
}





