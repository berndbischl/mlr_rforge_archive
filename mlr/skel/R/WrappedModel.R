#' Induced model of learner.
#'
#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the subset used for training, features used for training and computtation time for training.
#' 
#' Object slots: 
#' \describe{
#'	\item{learner [\code{\link{Learner}}]}{Learner that was used to fit the model.}
#'	\item{learner.model [any]}{Underlying model from used R package. If model fitting failed.... FIXME}
#'	\item{task.desc [\code{\link{TaskDesc}}]}{Description object of task.}
#'	\item{subset [\code{integer}]}{Subset used for training.}
#'	\item{time [\code{numeric}]}{Computation time for model fit in seconds.}
#' }
#' @name WrappedModel
#' @rdname WrappedModel
NULL
#FIXME really store the features? what abou varsel?

makeWrappedModel = function(learner, model, task.desc, subset, features, time) {
  UseMethod("makeWrappedModel")
}
  
makeWrappedModel.Learner = function(learner, model, task.desc, subset, features, time) {
  if(is.error(model)) {
    model = as.character(model)
    time = as.numeric(NA)
    cl = "FailureModel"
  } else {
    cl = "WrappedModel"
    structure(list(
      learner = learner,
      learner.model = model,
      task.desc = task.desc,
      subset = subset,
      features = features,
      time = time
    ), class=cl)
  }
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
