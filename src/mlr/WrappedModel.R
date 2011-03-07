#' @include object.r
roxygen()
#' @include task.desc.r
roxygen()
#' @include Learner.R
roxygen()
#' @include prepare.df.r
roxygen()


#' Result from \code{\link{train}}. It internally stores the underlying fitted model,
#' the IDs of the subset used for training, variables used for training and    
#' information about second-level optimization like tuned hyperparameters or selected variables. 
#' 
#' Getter.\cr
#' Note that all getters of \code{\linkS4class{task.desc}} can also be used. 
#' 
#' \describe{
#'	\item{learner [{\linkS4class{Learner}}]}{Learner that was used to fit the model.}
#'	\item{learner model [any]}{Underlying model from used R package.}
#'	\item{subset [integer]}{Subset used for training.}
#'	\item{fail [NULL | string]}{Generally NULL but if the training failed, the error message of the underlying train function.}
#'	\item{prepare.control [\code{\linkS4class{prepare.control}}]}{Control object used for preparing the training data.frame.}
#' }
#' 
#' @title Induced model of learner.
 
setClass(
		"WrappedModel",
		contains = c("object"),
		representation = representation(
				learner = "Learner",
				learner.model = "ANY",
        prep.control = "prepare.control",					
        desc = "task.desc",
				subset = "integer",
				vars = "character",
				time = "numeric"
		)
)

#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("WrappedModel"),
  def = function(.Object, learner, model, task.desc, prep.control, subset, vars, time) {
    if (missing(learner))
      return(make.empty(.Object))
    .Object@learner = learner
    .Object@learner.model = model
    .Object@desc = task.desc
    .Object@prep.control = prep.control
    .Object@subset = subset
    .Object@vars = vars
    .Object@time = time
    return(.Object)
  }
)



#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("WrappedModel"),
		def = function(x) {
			return(
					paste(
							"Learner model for id=", x@learner@desc@id, " class=", class(x@learner), "\n",  
							"Trained on obs: ", length(x@subset), "\n",
              "Used features: ", length(x@vars), "\n",
              "Hyperparameters: ", x@learner["par.vals.string"],
							sep=""
					)
			)
		}
)


#' Getter.
#' @rdname WrappedModel-class

setMethod(
		f = "[",
		signature = signature("WrappedModel"),
		def = function(x,i,j,...,drop) {
			args = list(...)
			
			y = x@desc[i]
			if (!is.null(y))
				return(y)

			callNextMethod()
		}
)













