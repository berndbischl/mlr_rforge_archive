setGeneric(
  name = "makeWrappedModel",
  def = function(learner, model, task.desc, subset, vars, time) {
    standardGeneric("makeWrappedModel")
  }
)

setMethod(
  f = "makeWrappedModel",
  signature = signature(learner="Learner"),
  
  def = function(learner, model, task.desc, subset, vars, time) {
    if(is(model, "try-error")) {
      msg = as.character(model)
      if (.mlr.local$errorhandler.setup$on.learner.error == "warn")
        warning("Could not train the learner: ", msg) 
      new("FailureModel", learner, msg, task.desc, subset, vars, as.numeric(NA))
    } else {
      # create normal model
      new("WrappedModel", learner, model, task.desc, subset, vars, time)    
    }
  }
)


