makeWrappedModel = function(learner, model, task.desc, prep.control, subset, vars, time) {
  # if error happened we use a failure model
  if(is(model, "try-error")) {
    msg = as.character(model)
    if (.mlr.local$errorhandler.setup$on.learner.error == "warn")
      warning("Could not train the learner: ", msg) 
    m = new("failure.model", learner, msg, task.desc, prep.control, subset, vars, as.numeric(NA))
  } else if(is(learner, "OptWrapper")) {
    or = attr(model, "opt.result")
    attr(model, "opt.result") = NULL
    m = new("opt.model", learner, model@learner.model, task.desc, prep.control, subset, vars, time, or)  
  } else if(is(learner, "PreprocWrapper")) {
    ctrl = attr(model, "control")
    attr(model, "control") = NULL
    m = new("preproc.model", learner, model, task.desc, prep.control, subset, vars, time, ctrl)  
  } else if(is(learner, "FilterWrapper")) {
    vars = attr(model, "filter.result")
    attr(model, "filter.result") = NULL
    m = new("WrappedModel", learner, model, task.desc, prep.control, subset, vars, time)  
  } else {
    # create normal model
    m = new("WrappedModel", learner, model, task.desc, prep.control, subset, vars, time)    
  }
  return(m)
}
