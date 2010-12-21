make.wrapped.model = function(learner, model, data.desc, task.desc, prep.control, hps, subset, vars, time) {
  # if error happened we use a failure model
  if(is(model, "try-error")) {
    msg = as.character(model)
    if (.mlr.local$errorhandler.setup$on.learner.error == "warn")
      warning("Could not train the learner: ", msg) 
    m = new("failure.model", learner, msg, data.desc, task.desc, prep.control, subset, vars, as.numeric(NA))
  } else if(is(learner, "opt.wrapper")) {
    or = attr(model, "opt.result")
    attr(model, "opt.result") = NULL
    m = new("opt.model", learner, model, data.desc, task.desc, prep.control, subset, vars, time, or)  
  } else {
    # create normal model
    m = new("wrapped.model", learner, model, data.desc, task.desc, prep.control, subset, vars, time)    
  }
  return(m)
}
