
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# be sel best usw.




varsel.seq = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	all.vars = task["input.names"]
	path = list()
	
	start.vars = switch(method,
			sfs = character(0),
			sbs = all.vars,
			sffs = character(0),
			sfbs = all.vars,
			stop(paste("Unknown method:", method))
	) 
	
	gen.new.states = switch(method,
			sfs = gen.new.states.sfs,
			sbs = gen.new.states.sbs,
			sffs = gen.new.states.sfs,
			sfbs = gen.new.states.sbs,
			stop(paste("Unknown method:", method))
	) 
	
	state = eval.state(learner, task, resampling, measures, aggr, vars=start.vars)
	
	path[[length(path)+1]] = state		
	
	compare = compare.diff
	
	forward = (method %in% c("sfs", "sffs"))
	
	while (TRUE) {
		logger.debug("current:")
		logger.debug(state$vars)
		#print("current:")
		#print(state$vars)
		#cat("forward:", forward, "\n")
		s = seq.step(learner, task, resampling, measures, aggr, control, forward, all.vars, state, gen.new.states, compare)	
		#print(s$rp$measures["mean", "mmce"])
		if (is.null(s)) {
			break;
		} else {
			state = s
		}
		
		path[[length(path)+1]] = state		
		
		while (method %in% c("sffs", "sfbs")) {
			#cat("forward:", !forward, "\n")
			gns = switch(method,
					sffs = gen.new.states.sbs,
					sfbs = gen.new.states.sfs
			) 
			s = seq.step(learner, task, resampling, measures, aggr, control, !forward, all.vars, state, gns, compare)	
			if (is.null(s)) {
				break;
			} else {
				state = s
			}
			path[[length(path)+1]] = state		
		}
		
	}
	return(list(best=state, path=path))
}

seq.step = function(learner, task, resampling, measures, aggr, control, forward, all.vars, state, gen.new.states, compare) {
	not.used = setdiff(all.vars, state$vars)
	new.states = gen.new.states(state$vars, not.used)
	if (length(new.states) == 0)
		return(NULL)
	vals = list()
		
	es = eval.states(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, varsets=new.states)
	#print(unlist(vals))
	
	s = select.best.state(es, control, measures, aggr)
	if (compare(state, s, control, measures, aggr, forward))
		return(s)
	return(NULL)
}

gen.new.states.sfs = function(vars, not.used) {
	new.states = list()
	for (i in 1:length(not.used)) {
		#cat(not.used[i], " ")
		new.states[[i]] = c(vars, not.used[i])
	}
	#cat("\n")
	return(new.states)
}


gen.new.states.sbs = function(vars, not.used) {
	new.states = list()
	for (i in 1:length(vars)) {
		new.states[[i]] = setdiff(vars, vars[i])
	}
	#cat("\n")
	return(new.states)
}




