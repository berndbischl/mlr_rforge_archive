
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# be sel best usw.


sel.control <- function(compare="diff", alpha=0.01, beta=0.01, steps=Inf) {
	list(compare=compare, alpha=alpha, beta=beta, steps=steps, minimize=TRUE)
}



varsel.seq = function(learner, task, resampling, measures, aggr, method, control=sel.control()) {
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
	states = list()
	vals = list()
	
		
	es = eval.varsets(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, varsets=new.states)
	
	for (i in 1:length(new.states)) {
		new.vars = new.states[[i]]
		states[[i]] = eval.state(learner, task, resampling, measures, aggr, new.vars)
		vals[[i]] = compare(state, states[[i]], control, measures, aggr)
	}
	print(unlist(vals))
	select.best(states, vals, control, forward)
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


get.perf = function(state, measures, aggr) {
	rp = state$rp
	m = names(measures)[1]
	a = names(aggr)[1]		
	rp$measures[a, m]
}



compare.diff = function(state1, state2, control, measures, aggr) {
	m1 = get.perf(state1, measures, aggr)
	m2 = get.perf(state2, measures, aggr)
	m1 - m2
}

select.best.diff = function(states, vals, control, forward) {
	if (control$minimize)
		i = which.max(vals)
	else 
		i = which.min(vals)
	d = vals[[i]]
	if (forward && d > control$alpha)
		return(states[[i]])
	if (!forward && d > control$beta)
		return(states[[i]])
	return(NULL)
}


#backward.sel <- function(learn.task, steps=1, resample.desc, ranges=list(), eps=0.001) {
#	is <- learn.task["input.names"]
#	rin <- make.resample.instance(size=learn.task["size"]), desc=resample.desc)
#state <- eval.state(learn.task, rin, vars=is)
#
#while (TRUE) {
#	print("current:")
#	print(state)
#	rin <- make.resample.instance(size=learn.task["size"], desc=resample.desc)
#	best.kid.state <- NA 
#	for (v in state$vars) {
#		new.vars <- setdiff(state$vars, v)
#		new.state <- eval.state(learn.task, rin, vars=new.vars)
#		if (is.na(best.kid.state) || best.kid.state$perf > new.state$perf) {
#			best.kid.state <- new.state
#		}
#	}
#	print(best.kid.state)
#	if (best.kid.state$perf - state$perf < eps) {
#		state <- best.kid.state
#	} else {
#		break
#	}
#}
#return(state)
#}


eval.states = function(learner, task, resampling, measures, aggr, states) {
	mylapply	
}


eval.state <- function(learner, task, resampling, measures, aggr, vars) {
	rf = resample.fit(learner=learner, task=task, resampling=resampling, vars=vars)
	rp = performance(rf, measures=measures, aggr=aggr, task=task)
	return(list(vars=vars, rp=rp))
}


