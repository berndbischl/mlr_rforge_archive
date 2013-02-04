#FIXME: should we really define validation error like this?
tuneIrace = function(learner, task, resampling, measures, par.set, control,
                   opt.path, show.info, log.fun) {
  
  requirePackages(c("irace"), "tuneIrace")
  #mbo.control = control$mbo.control
  # set final evals to 0 to save time. we dont really need final evals in this context.
  #mbo.control$final.evals = 0L
  #cx = identity
  
  tff = function(x) tunerFitnFun(x, learner=learner, task=task, resampling=resampling, measures=measures, 
    par.set=par.set, ctrl=control, opt.path=opt.path, show.info=show.info, 
    log.fun=log.fun, trafo=TRUE, convertx=cx)    
  
  or = irace(tff, par.set, des=NULL, learner=control$learner, control=mbo.control, show.info=FALSE)
  
  makeTuneResult(learner, control, x, or$y, opt.path)
}
