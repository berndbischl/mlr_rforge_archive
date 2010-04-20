#' Create learner object. 
#' 
#' @param class [string] \cr
#'   	  Class of learner to create.
#' @param id [string]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param label [string]\cr 
#'        Label string for object. Used in plots, etc.  
#' @param ... [any] \cr
#'        Optional named (hyper)parameters.
#' 
#' @return \code{\linkS4class{wrapped.learner}}.
#' 
#' @usage make.learner(class, id, label, ...)
#' 
#' @export
#' 
make.learner = function(class, id, label, ...) {
	parset = list(...)
	wl = new(class, parset=parset)
	if (!missing(id))
		wl@id = id
	if (!missing(label))
		wl@label = label
	return(wl)
}





