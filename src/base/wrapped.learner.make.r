#' Create learner object. 
#' 
#' @param name [string] \cr
#'   Name of learner.
#' @param ... [any] \cr
#'  Optional named (hyper)parameters.
#' 
#' @return \code{\linkS4class{wrapped.learner}}.
#' 
#' @export
#' 
make.learner = function(type, id, label, ...) {
	parset = list(...)
	wl = new(type, parset=parset)
	if (!missing(id))
		wl@id = id
	if (!missing(label))
		wl@label = label
	return(wl)
}





