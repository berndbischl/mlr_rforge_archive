#' @include prediction.r
roxygen()


#' Converts predictions to a format package ROCR can handle.
#' 
#' @param x [character] \cr
#'		Predictions. 			
#' 
#' @exportMethod as.ROCR.preds 
#' @rdname as.ROCR.preds 
#' @title Convert to ROCR format.


setGeneric(
		name = "as.ROCR.preds",
		def = function(x) {
			if(!require(ROCR)) {
				stop(paste("Package ROCR is missing!"))
			}
			standardGeneric("as.ROCR.preds")
		}
)

#' @export
#' @rdname as.ROCR.preds 
setMethod(
		f = "as.ROCR.preds",
		signature = signature(x="prediction"), 
		def = function(x) {
			td = x@task.desc
			dd = x@data.desc
			if(dd["class.nr"] != 2) {
				stop("More than 2 classes!")
			}
			p = x["prob"] 
			if(is.null(p)) {
				stop("No probabilities in prediction object!")
			}
			make.ROCR.pred(p, x["truth"], label.ordering=c(td["negative"], td["positive"]))
		}
)
