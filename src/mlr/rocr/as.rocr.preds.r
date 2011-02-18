#' @include Prediction.R
roxygen()
#' @include ResamplePrediction.R
roxygen()


#' Converts predictions to a format package ROCR can handle.
#' 
#' @param x [\code{\linkS4class{Prediction}}] \cr
#'		Prediction object. 			
#' 
#' @exportMethod as.ROCR.preds 
#' @rdname as.ROCR.preds 
#' @title Convert to ROCR format.


setGeneric(
		name = "as.ROCR.preds",
		def = function(x) {
#			if(!require(ROCR)) {
#				stop(paste("Package ROCR is missing!"))
#			}
			standardGeneric("as.ROCR.preds")
		}
)

#' @export
#' @rdname as.ROCR.preds 
setMethod(
		f = "as.ROCR.preds",
		signature = signature(x="Prediction"), 
		def = function(x) {
			if(x@desc["class.nr"] != 2) {
				stop("More than 2 classes!")
			}
			p = getScore(x) 
			if(is.null(p)) {
				stop("No probabilities in prediction object!")
			}
			ROCR.prediction(p, x@df$truth, label.ordering=c(x@desc["negative"], x@desc["positive"]))
		}
)


#' @export
#' @rdname as.ROCR.preds 
setMethod(
  f = "as.ROCR.preds",
  signature = signature(x="ResamplePrediction"), 
  def = function(x) {
    if(x@desc["class.nr"] != 2) {
      stop("More than 2 classes!")
    }
    if(x@type != "prob") {
      stop("No probabilities in prediction object!")
    }
    prob = getScore(x)
    iter = as.factor(x["iter"])
    prob = split(prob, iter)
    truth = split(x@df$truth, iter)
    ROCR.prediction(prob, truth, label.ordering=c(x@desc["negative"], x@desc["positive"]))
  }
)



