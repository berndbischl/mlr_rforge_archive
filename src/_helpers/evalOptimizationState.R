# evaluates either a hyperpar setting or a feature set by resampling
# must be already in correct format, either a named list of values or a named integer vector for features
# logs point and results
evalOptimizationState = function(learner, task, resampling, measures, par.set, bits.to.features, control, opt.path, log.fun, state) {
  if (is(control, "TuneControl")) 
    learner = setHyperPars(learner, par.vals=state)
  if (is(control, "VarselControl"))
    task = subsetData(task, vars=bits.to.features(state, task))
  r = resample(learner, task, resampling, measures=measures, show.info=FALSE)
  y = r$aggr
  log.fun(learner, task, resampling, measures, par.set, control, opt.path, state, y)
  return(y)
}
  
# evaluates a list of states by calling evalOptimizationState
# might be done in parallel
# adds points to path
evalOptimizationStates = function(learner, task, resampling, measures, par.set, bits.to.features, control, opt.path, log.fun, states, dobs, eols) {
  n = length(states)
  if (length(dobs) == 1)
    dobs = rep(dobs, n)
  if (length(eols) == 1)
    eols = rep(eols, n)
  # be sure to log only once on master
  log.fun2 = log.fun
  # if we parallelize the following lapply, we log afterwards and not on slave
  if (.mlr.conf$parallel.setup$mode != "local" && .mlr.conf$parallel.setup$level == "opt")
    log.fun2 = function(...) {}
  ys = mylapply(states, evalOptimizationState, from="opt", learner=learner, task=task, resampling=resampling, measures=measures, par.set=par.set,
    bits.to.features=bits.to.features, control=control, opt.path=opt.path, log.fun=log.fun2)
  # if we parallelized, we log afterwards: now
  if (.mlr.conf$parallel.setup$mode != "local" && .mlr.conf$parallel.setup$level == "opt") {
    Map(function(state, y) log.fun(learner, task, resampling, measures, par.set, control, opt.path, state, y),
      states, ys)
   }   
   # add stuff to opt.path
  for (i in seq_len(n)) 
    addOptPathEl(opt.path, x=as.list(states[[i]]), y=ys[[i]], dob=dobs[i], eol=eols[i])
}
