
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# todo maximize!!!!!!!!!!!!!!!!!!!!!!
# be sel best usw.



# todo: maxit
varsel.seq = function(learner, task, resampling, measures, aggr, method, control=varsel.control()) {
	
	seq.step = function(forward, state, gen.new.states, compare) {
		not.used = setdiff(all.vars, state$par)
		new.states = gen.new.states(state$par, not.used)
		if (length(new.states) == 0)
			return(NULL)
		vals = list()
		
		event = ifelse(forward, "forward", "backward")
		
		es = eval.states.varsel(learner=learner, task=task, resampling=resampling, 
				measures=measures, aggr=aggr, pars=new.states, event=event)
		#print(unlist(vals))
		
		s = select.best.state(es, control)
		thresh = ifelse(forward, control$alpha, control$beta)
		if (!compare(state, s, control, measures, aggr, thresh))
			s = NULL
		path <<- add.path.els.varsel(path, es, s)
		return(list(path=path, state=s))
	}
	
	gen.new.states.sfs = function(vars, not.used) {
		new.states = list()
		for (i in seq(along=not.used)) {
			#cat(not.used[i], " ")
			new.states[[i]] = c(vars, not.used[i])
		}
		#cat("\n")
		return(new.states)
	}
	
	
	gen.new.states.sbs = function(vars, not.used) {
		new.states = list()
		for (i in seq(along=not.used)) {
			new.states[[i]] = setdiff(vars, vars[i])
		}
		#cat("\n")
		return(new.states)
	}
	
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
	
	state = eval.state.varsel(learner, task, resampling, measures, aggr, par=start.vars, event="start")
	
	path = add.path.varsel(path, state, accept=T)		
	
	compare = compare.diff
	
	forward = (method %in% c("sfs", "sffs"))
	
	while (TRUE) {
		print(path)
		
		logger.debug("current:")
		logger.debug(state$par)
		#print("current:")
		#print(state$par)
		#cat("forward:", forward, "\n")
		s = seq.step(forward, state, gen.new.states, compare)	
		#print(s$rp$measures["mean", "mmce"])
		if (is.null(s$state)) {
			break;
		} else {
			state = s$state
		}
		
		while (method %in% c("sffs", "sfbs")) {
			#cat("forward:", !forward, "\n")
			gns = switch(method,
					sffs = gen.new.states.sbs,
					sfbs = gen.new.states.sfs
			) 
			s = seq.step(!forward, all.vars, state, gns, compare)
			if (is.null(s$state)) {
				break;
			} else {
				state = s$state
			}
		}
	}
	new("opt.result", opt=make.path.el(state), path=path)
}





