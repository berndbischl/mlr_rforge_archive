# todo: maxit, max.vars
# todo: compare relative
varsel.seq = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, log.fun) {
	
	seq.step = function(forward, state, gen.new.states, compare) {
    xs = gen.new.states(state$x)
		if (length(xs) == 0)
			return(NULL)
		
		eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, xs)
		
		best = getBestElement(opt.path, eol=NA)
		thresh = ifelse(forward, control["alpha"], control["beta"]) 
		# if backward step and we have too many vars we do always go to the next best state with one less var.
    if (!compare(state, best, control, measures[[1]], thresh) && (forward || sum(x) < control["max.vars"]))
      return(NULL)
    setEoL(opt.path, state$x, NA)
    return(best)
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
  
  dim = task["dim"]
	compare = compare.diff
	method = control["method"]
	
	x = switch(method,
			sfs = rep(0, dim),
			sbs = rep(1, dim),
			sffs = rep(0, dim),
			sfbs = rep(1, dim),
			stop(paste("Unknown method:", method))
	) 
	
	gen.new.states = switch(method,
			sfs = gen.new.states.sfs,
			sbs = gen.new.states.sbs,
			sffs = gen.new.states.sfs,
			sfbs = gen.new.states.sbs,
			stop(paste("Unknown method:", method))
	) 
	
	y = eval.rf(learner, task, resampling, measures, NULL, bits.to.features, control, x)
  state = list(x=x, y=y)
  path = addPathElement(opt.path, x=x, y=y)		
	
	forward = (method %in% c("sfs", "sffs"))
	while (!is.null(state)) {
    logger.debug("current:")
    logger.debug(state$x)
		#cat("forward:", forward, "\n")
		# if forward step and we habe enuff features: stop
		if (forward && control["max.vars"] <= sum(state$x))
			break
		state = seq.step(forward, state, gen.new.states, compare)	
		#print(s$rp$measures["mean", "mmce"])
		if (is.null(state)) 
			break;
		
		while (method %in% c("sffs", "sfbs")) {
			#cat("forward:", !forward, "\n")
			gns = switch(method,
					sffs = gen.new.states.sbs,
					sfbs = gen.new.states.sfs
			) 
			# if forward step and we habe enuff features: stop
			if (!forward && control["max.vars"] <= sum(state$x))
				break
			state = seq.step(!forward, state, gns, compare)
      if (is.null(state)) 
        break;
		}
	}

  e = getBestElement(opt.path, measureAggrNames(measures[[1]])[1])
  new("opt.result", learner, control, bits.to.features(e$x, task), e$y, opt.path)
}





