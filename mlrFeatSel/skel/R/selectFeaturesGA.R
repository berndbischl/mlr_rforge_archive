# FIXME use max.features

## control should be a list including init, size, rate and maxit or threshold:
## init = number of samples for the initial population
## size = number of samples to be generated in each GA-step
## rate = mutation rate, i.e. probability of flipping a feature
## maxit = maximum number of iterations
## threshold = value for the fitness function

selectFeaturesGA = function(learner, task, resampling, measures, bit.names, bits.to.features, control, opt.path, show.info) {
  fit = mlr:::measureAggrName(measures[[1]])
  states = lapply(1:control$init, 
	  function(i) rbinom(length(getTaskFeatureNames(task)), 1, control$rate))
  evalOptimizationStates(learner, task, resampling, measures, 
	  bits.to.features, control, opt.path, show.info, states, 0L, as.integer(NA))
  for(i in 1:control$maxit) {
	  # parents = select all from OP that are alive
		# for j in 1:lambda
	  #   select 2 parents randomly
	  #   CX + mutate --> kid
	  # eval all kids  
	  # select mu best from parent, kids; all others are set EOL = i
		
    index = rank(as.data.frame(opt.path)[[fit]], ties.method = "random")[1:control$size]
    e = getOptPathEl(opt.path, index)
    states = lapply(1:control$size, function(i) as.integer(BitMutation(as.data.frame(e$x)[i,], control$rate)))
    evalOptimizationStates(learner, task, resampling, measures, 
		  bits.to.features, control, opt.path, show.info, states, i, as.integer(NA))
  }
  i = getOptPathBestIndex(opt.path, mlr:::measureAggrName(measures[[1]]), ties="random")
  e = getOptPathEl(opt.path, i)
	makeFeatSelResult(learner, control, names(e$x)[e$x == 1], e$y, opt.path)
}

