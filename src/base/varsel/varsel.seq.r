
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# be sel best usw.




varsel.seq = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	all.vars = task["input.names"]
	
	
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
	
	compare = compare.diff
	select.best = select.best.diff
	
	forward = (method %in% c("sfs", "sffs"))
	
	while (TRUE) {
		logger.debug("current:")
		logger.debug(state$vars)
		print("current:")
		print(state$vars)
		logger.debug(state$perf$aggr)
		cat("forward:", forward, "\n")
		s = seq.step(learner, task, resampling, measures, aggr, control, forward, all.vars, state, gen.new.states, compare, select.best)	
		print(s$rp$measures["mean", "mmce"])
		if (is.null(s)) {
			break;
		} else {
			state = s
		}
		
		
		while (method %in% c("sffs", "sfbs")) {
			cat("forward:", !forward, "\n")
			gns = switch(method,
					sffs = gen.new.states.sbs,
					sfbs = gen.new.states.sfs
			) 
			s = seq.step(learner, task, resampling, measures, aggr, control, !forward, all.vars, state, gns, compare, select.best)	
			if (is.null(s)) {
				break;
			} else {
				state = s
			}
		}
		
	}
	return(state)
}

seq.step = function(learner, task, resampling, measures, aggr, control, forward, all.vars, state, gen.new.states, compare, select.best) {
	not.used = setdiff(all.vars, state$vars)
	new.states = gen.new.states(state$vars, not.used)
	if (length(new.states) == 0)
		return(NULL)
	vals = list()
		
	es = eval.states(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, varsets=new.states)
	
	print(unlist(vals))
	select.best(es, vals, control, forward)
}

gen.new.states.sfs = function(vars, not.used) {
	new.states = list()
	for (i in 1:length(not.used)) {
		cat(not.used[i], " ")
		new.states[[i]] = c(vars, not.used[i])
	}
	cat("\n")
	return(new.states)
}


gen.new.states.sbs = function(vars, not.used) {
	new.states = list()
	for (i in 1:length(vars)) {
		new.states[[i]] = setdiff(vars, vars[i])
	}
	cat("\n")
	return(new.states)
}




