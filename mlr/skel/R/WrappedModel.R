#FIXME really store the features? what abou varsel?

#' Induced model of learner.
#'
#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the subset used for training, features used for training and computtation time for training.
#' 
#' The constructer \code{makeWrappedModel} is only for internal use.
#'
#' Object members: See arguments.
#'
#' @param learner [\code{\link{Learner}}]\cr 
#'   The learner.  
#' @param model [any]\cr 
#'   Underlying model.   
#' @param task.desc [\code{\link{TaskDesc}}]\cr 
#'   Task description object.
#' @param subset [\code{integer}]\cr 
#'   Subset used for training.
#' @param features [\code{character}]\cr 
#'   Features used for training.
#' @param time [\code{numeric(1)}]\cr 
#'   Computation time for model fit in seconds.
#' @return [\code{\link{WrappedModel}}]. 
#' @export
#' @aliases WrappedModel
makeWrappedModel = function(learner, model, task.desc, subset, features, time) {
  UseMethod("makeWrappedModel")
}
  
#' @S3method makeWrappedModel Learner
makeWrappedModel.Learner = function(learner, model, task.desc, subset, features, time) {
  if(is.error(model)) {
    model = as.character(model)
    time = as.numeric(NA)
    cl = c("FailureModel", "WrappedModel")
  } else {
    cl = "WrappedModel"
  }
  structure(list(
    learner = learner,
    learner.model = model,
    task.desc = task.desc,
    subset = subset,
    features = features,
    time = time
  ), class=cl)
}

#' @S3method print WrappedModel
print.WrappedModel = function(x, ...) {
  cat(
    "Learner model for id=", x$learner$id, " class=", class(x$learner)[1], "\n",  
    "Trained on obs: ", length(x$subset), "\n",
    "Used features: ", length(x$features), "\n",
    "Hyperparameters: ", getHyperParsString(x$learner), "\n",
    sep=""
  )
}
