#' @include TaskDesc.R
roxygen()
#' @include Learner.R
roxygen()


#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the IDs of the subset used for training, variables used for training and    
#' information about second-level optimization like tuned hyperparameters or selected variables. 
#' 
#' Getter.\cr
#' Note that all getters of \code{\linkS4class{TaskDesc}} can also be used. 
#' 
#' \describe{
#'	\item{learner [{\linkS4class{Learner}}]}{Learner that was used to fit the model.}
#'	\item{learner model [any]}{Underlying model from used R package.}
#'	\item{subset [\code{integer}]}{Subset used for training.}
#'	\item{fail [NULL | string]}{Generally NULL but if the training failed, the error message of the underlying train function.}
#' }
#' @exportClass WrappedModel
#' @title Induced model of learner.
 
setClass(
		"WrappedModel",
		representation = representation(
				learner = "Learner",
				learner.model = "ANY",
        task.desc = "TaskDesc",
				subset = "integer",
				vars = "character",
				time = "numeric"
		)
)

#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("WrappedModel"),
  def = function(.Object, learner, model, task.desc, subset, vars, time) {
    if (missing(learner))
      return(make.empty(.Object))
    .Object@learner = learner
    .Object@learner.model = model
    .Object@task.desc = task.desc
    .Object@subset = subset
    .Object@vars = vars
    .Object@time = time
    return(.Object)
  }
)


setMethod("show", "WrappedModel", function(object) {
  cat(
    "Learner model for id=", object@learner@id, " class=", class(object@learner), "\n",  
    "Trained on obs: ", length(object@subset), "\n",
    "Used features: ", length(object@vars), "\n",
    "Hyperparameters: ", getHyperParsString(object@learner), "\n",
    sep=""
  )
})
