# todo: maxit, max.vars
# todo: compare relative
varsel.seq = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, log.fun) {
  
  seq.step = function(forward, state, gen.new.states, compare) {
    # we have too many vars already and cannot move forward
    if (forward && control@max.vars <= sum(unlist(state$x)))
      return(NULL)
    xs = gen.new.states(state$x)
    if (length(xs) == 0)
      return(NULL)
    dob = max(opt.path@env$dob) + 1L
    # die at once
    eval.states(learner, task, resampling, measures, NULL, bits.to.features, control, opt.path, xs, dob=dob, eol=dob)
    
    best.i = getBestIndex(opt.path, dob=dob, ties="random")
    best = getPathElement(opt.path, best.i)
    # best element lives one iteration longer
    thresh = ifelse(forward, control@alpha, control@beta) 
    better = compare(state, best, control, measures[[1]], thresh) 
    # if backward step and we have too many vars we do always go to the next best state with one less var.
    if ((forward && better) || (!forward && (better || sum(unlist(state$x)) > control@max.vars))) {
      mlrTune:::setEoL(opt.path, best.i, dob+1)
      return(best)
    } else {
      return(NULL)
    }
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
  
  dim = length(bit.names)
  compare = compare.diff
  method = control@method
  
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
  
  y = mlr:::eval.rf(learner, task, resampling, measures, NULL, bits.to.features, control, x)
  state = list(x=x, y=y)
  path = addPathElement(opt.path, x=as.list(x), y=y, dob=1L, eol=2L)   
  
  forward = (method %in% c("sfs", "sffs"))
  fail = 0
  while ((method %in% c("sfs", "sbs")  && fail == 0) || (method %in% c("sffs", "sfbs") && fail < 2)) {
    logger.debug("current:")
    logger.debug(state$x)
    #cat("forward:", forward, "\n")
    state2 = seq.step(forward, state, gen.new.states, compare)
    #print(s$rp$measures["mean", "mmce"])
    # we could not move to state2 in normal step, stay where we are
    if (!is.null(state2)) {
      state = state2
      fail = 0
    } else {
      fail = fail + 1
    }
    
    if (method %in% c("sffs", "sfbs")) {
      #cat("forward:", !forward, "\n")
      gns = switch(method,
        sffs = gen.new.states.sbs,
        sfbs = gen.new.states.sfs
      ) 
      state2 = seq.step(!forward, state, gns, compare)
      if (!is.null(state2)) {
        state = state2
        fail = 0
      } else {
        fail = fail + 1
      }
    }
  }
  # if last generation contains no better element, go to second to last
  last = max(opt.path@env$dob) 
  j = which(opt.path@env$dob == last)
  if (all(opt.path@env$eol[opt.path@env$dob == last] == last))
    last = last-1
  i = getBestIndex(opt.path, measureAggrNames(measures[[1]])[1], dob=last, ties="first")
  e = getPathElement(opt.path, i)
  new("OptResult", learner, control, bits.to.features(e$x, task), e$y, opt.path)
}





