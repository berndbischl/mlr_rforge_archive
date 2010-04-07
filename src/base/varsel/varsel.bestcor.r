
varsel.bestcor = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	all.vars = task["input.names"]
	path = list()
	
	start.vars = character(0)	
	state = eval.state(learner, task, resampling, measures, aggr, vars=start.vars)
	
	path[[length(path)+1]] = state		
	data = task["data"]
	not.used <<- all.vars
	cors <<- abs(cor(data[, all.vars], data[, task["target"]])) 
	o <<- order(cors, decreasing=T)
	not.used = not.used[o]
	print(cors)
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
			s = eval.state(learner, task, resampling, measures, aggr, vars=new.vars)
			if (compare.diff(state, s, control, measures, aggr, forward=T)) {
				state=s
				path[[length(path)+1]] = state
				not.used = not.used[-i]
				found = TRUE
				break
			}
			
		}
		if (!found)
			break
	}
	return(list(best=state, path=path))
}





