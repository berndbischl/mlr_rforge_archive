#' @include Prediction.R
roxygen()
#' @include ResamplePrediction.R
roxygen()


#' Converts predictions to a format package ROCR can handle.
#' 
#' @param x [\code{\linkS4class{Prediction}}] \cr
#'		Prediction object. 			
#' 
#' @exportMethod as.ROCR.prediction 
#' @rdname as.ROCR.prediction 
#' @title Convert to ROCR format.


setGeneric(
		name = "as.ROCR.prediction",
		def = function(x) {
#			if(!require(ROCR)) {
#				stop(paste("Package ROCR is missing!"))
#			}
			standardGeneric("as.ROCR.prediction")
		}
)

#' @export
#' @rdname as.ROCR.prediction 
setMethod(
		f = "as.ROCR.prediction",
		signature = signature(x="Prediction"), 
		def = function(x) {
			if(length(x@task.desc@class.levels) != 2) {
				stop("More than 2 classes!")
			}
			p = getProb(x) 
			if(is.null(p)) {
				stop("No probabilities in prediction object!")
			}
			ROCR.prediction(p, x@df$truth, label.ordering=c(x@task.desc@negative, x@task.desc@positive))
		}
)


#' @export
#' @rdname as.ROCR.prediction 
setMethod(
  f = "as.ROCR.prediction",
  signature = signature(x="ResamplePrediction"), 
  def = function(x) {
    if(length(x@task.desc@class.levels) != 2) {
      stop("More than 2 classes!")
    }
    if(x@type != "prob") {
      stop("No probabilities in prediction object!")
    }
    prob = getProb(x)
    iter = as.factor(x["iter"])
    prob = split(prob, iter)
    truth = split(x@df$truth, iter)
    ROCR.prediction(prob, truth, label.ordering=c(x@task.desc@negative, x@task.desc@positive))
  }
)



