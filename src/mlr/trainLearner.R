#' Mainly for internal use. Trains a wrapped learner on a given training set, 
#' w.r.t. some hyperparameters and case weights.
#' You have to implement this method if you want to add another learner to this package. 
#' @param .learner [\code{\linkS4class{Learner}}] \cr  
#'   Wrapped learner from this package. 
#' @param .task [\code{\linkS4class{LearnTask}}] \cr
#'   Task to train learner on.
#' @param .subset [integer] \cr
#'   Subset of cases, index task with this.
#' @param ... [any] \cr
#' 		  Additional parameters, which need to be passed to the underlying train function.
#' 		    
#' @return Model of the underlying learner.
#' 
#' @exportMethod trainLearner
#' @seealso \code{\link{get.data}}
#' @rdname trainLearner
#' @title Internal training method for learner. 

setGeneric(
		name = "trainLearner",
		def = function(.learner, .task, .subset,  ...) {
			standardGeneric("trainLearner")
		}
)

