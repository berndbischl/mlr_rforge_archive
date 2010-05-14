#' Create learner object. 
#' 
#' @param class [string] \cr
#'   	  Class of learner to create.
#' @param id [string]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param label [string]\cr 
#'        Label string for object. Used in plots, etc.
#' @param predict.type [string] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to predict.
#'        Default is "response". "decision" is experimental.
#' 		  Ignored for regression.	 
#' @param predict.threshold [numeric] \cr
#'        Threshold to produce class labels if type is not "response". 
#' 	      Currently only supported for binary classification and type="prob", where it represents the required predicted probability
#'        for the positive class, so that a positive class is predicted as "response".
#'        Default is 0.5 for type="prob".
#' 		  Ignored for regression.	 
#' @param ... [any] \cr
#'        Optional named (hyper)parameters.
#' 
#' @return \code{\linkS4class{wrapped.learner}}.
#' 
#' @usage make.learner(class, id, label, ...)
#' 
#' @export
#' 
make.learner = function(class, id, label, predict.type="response", predict.threshold=numeric(0), ...) {
	parset = list(...)
	wl = new(class,parset=parset)
	if (!missing(id))
		wl@id = id
	if (!missing(label))
		wl@label = label
	wl@predict.type = predict.type 
	wl@predict.threshold = predict.threshold 
	
	return(wl)
}





