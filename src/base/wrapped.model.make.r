make.wrapped.model = function(learner, model, data.desc, task.desc, subset, vars, time) {
  # if error happened we use a failure model
  if(is(learner.model, "try-error")) {
    msg = as.character(learner.model)
    if (.mlr.local$errorhandler.setup$on.learner.error == "warn")
      warning("Could not train the learner: ", msg) 
    learner.model <- new("learner.failure", msg=msg)
    time.train = as.numeric(NA)
    return(new("failure.model", learner, model, data.desc, task.desc, subset, vars, time))
  } 
  if(is(learner, "opt.wrapper")) {
    #set to "train" if not specified
  hyper.types = rep("train", length(hps))
  names(hyper.types) = names(hps)
  hyper.types = insert(hyper.types, wl["hyper.types"])
  
  # create normal model
  new("wrapped.model", learner = wl, learner.model = learner.model, 
    data.desc=task@data.desc, task.desc=task@task.desc, subset=subset, 
    vars=vars, time = time.train)  
}