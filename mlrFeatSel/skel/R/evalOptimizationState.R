#FIXME: read all
# evaluates  a feature set by resampling
# must be already in correct format, either a named list of values or a named integer vector for features
# logs point and results
evalOptimizationState = function(learner, task, resampling, measures, bits.to.features, control, opt.path, show.info, state) {
  task = subsetTask(task, features=bits.to.features(state, task))
  r = resample(learner, task, resampling, measures=measures, show.info=FALSE)
  y = r$aggr
  if (show.info)
    logFunSelFeatures(learner, task, resampling, measures, control, opt.path, state, y)
  return(y)
}

# evaluates a list of states by calling evalOptimizationState
# might be done in parallel
# adds points to path
evalOptimizationStates = function(learner, task, resampling, measures, bits.to.features, control, opt.path, show.info, states, dobs, eols) {
  n = length(states)
  if (length(dobs) == 1)
    dobs = rep(dobs, n)
  if (length(eols) == 1)
    eols = rep(eols, n)
  # FIXME better export
  ys = parallelMap(evalOptimizationState, states, level="mlrFeatSel", 
                   more.args=list(learner=learner, task=task, resampling=resampling,
                                  measures=measures, bits.to.features=bits.to.features, 
                                  control=control, opt.path=opt.path, show.info=show.info))
  # add stuff to opt.path
  for (i in seq_len(n)) 
    addOptPathEl(opt.path, x=as.list(states[[i]]), y=ys[[i]], 
                 dob=dobs[i], eol=eols[i], check.feasible=FALSE)
  return(ys)  
}