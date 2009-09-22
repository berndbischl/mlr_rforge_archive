#' Predicts the target values of a new data set based on 
#' an already fitted wrapped.model of a learn.task.   
#'
#' @param object [\code{\linkS4class{learn.task}}] \cr 
#'  Learning task
#' @param model [\code{\linkS4class{model}}] \cr 
#'   Wrapped model, trained from learn task  
#' @param newdata [\code{\link{data.frame}}] \cr 
#'   Contains new observations which should be predicted (by default the train data of the wrapped model).
#'
#' @return Object containing the predicted targets. See inherited methods. 
#'
#' @usage predict(object, model, newdata)
#' 
#' @seealso \code{\link{train}}, \code{\link{predict,classif.task-method}}, \code{\link{predict,regr.task-method}}, \code{\link{performance}}
#' 
#' @export
#' @title predict
#' @aliases predict

setMethod(
		f = "predict",
		signature = signature(object="learn.task"),
		def = function(object, model, newdata) {
			
		}
)
