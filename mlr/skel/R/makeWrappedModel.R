#' Induced model of learner.
#'
#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the subset used for training, features used for training and the training time.
#' 
#' \describe{
#'	\item{learner [\code{\link{Learner}}]}{Learner that was used to fit the model.}
#'	\item{learner.model [any]}{Underlying model from used R package.}
#'	\item{subset [\code{integer}]}{Subset used for training.}
#'	\item{fail [NULL | string]}{Generally NULL but if the training failed, the error message of the underlying train function.}
#' }
NULL

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

print.WrappedModel = function(x, ...) {
  cat(
    "Learner model for id=", x$learner$id, " class=", class(x$learner), "\n",  
    "Trained on obs: ", length(x$subset), "\n",
    "Used features: ", length(x$features), "\n",
    "Hyperparameters: ", getHyperParsString(x$learner), "\n",
    sep=""
  )
}
