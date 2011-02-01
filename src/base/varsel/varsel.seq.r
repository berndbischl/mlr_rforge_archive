# todo: maxit, max.vars
varsel.seq = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, log.fun) {
	
	seq.step = function(forward, x, gen.new.states, compare) {
		xs = gen.new.states(x)
		if (length(new.states) == 0)
			return(NULL)
		vals = list()
		
		eval.states(learner, task, resampling, measures, control, lapply(xs, bits.to.features))
		
		s = select.best.state(es, measures)
		if (forward)
			thresh = control["alpha"]
		# if backward step and we have too many vars we do always go to the next best state with one less var.
		else
			thresh = ifelse(length(state$par) <= control["max.vars"], control["beta"], -Inf)
		if (!compare(state, s, control, measures[[1]], thresh)) {
			s = NULL
      changed = "<>"
    } else {
      # symmetric diff for changed feature
      changed = setdiff(union(state$par, s$par), intersect(state$par, s$par))
    } 

    logger.info(level="varsel", paste("varsel: forward=",forward, " features=", length(state$par), " perf=", round(get.perf(state), 3), " feat=", changed, sep=""))      
    set.eol(opt.path, x) 
		return(list(path=path, state=s))
	}
	
	gen.new.states.sfs = function(x) {
    xs = list()
    for (i in 1:length(x))
      if (x[i] == 0) {
        y = x
        y[i] = 1
        xs[[length(xs)+1]] = y
      }
    xs
	}
	
	gen.new.states.sbs = function(x) {
    xs = list()
    for (i in 1:length(x))
      if (x[i] == 1) {
        y = x
        y[i] = 0
        xs[[length(xs)+1]] = y
      }
    xs
  }
  
  all.vars = getFeatureNames(task)
	
	method = control["method"]
	
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
	
	state = eval.rf(learner, task, resampling, measures, NULL, control, start.vars)
	
	path = add.path.varsel(path, state, accept=TRUE)		
	
	compare = compare.diff
	
	forward = (method %in% c("sfs", "sffs"))
	
	while (TRUE) {
		logger.debug("current:")
		logger.debug(state$par)
		#cat("forward:", forward, "\n")
		# if forward step and we habe enuff features: stop
		if (forward && control["max.vars"] <= length(state$par))
			break
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
			# if forward step and we habe enuff features: stop
			if (!forward && control["max.vars"] <= length(state$par))
				break
			s = seq.step(!forward, state, gns, compare)
			if (is.null(s$state)) {
				break;
			} else {
				state = s$state
			}
		}
	}

  e = getBestElement(opt.path, measureAggrNames(measures[[1]])[1])
  new("opt.result", learner, control, e$x, e$y, opt.path)
}





