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
make.learner = function(name, ...) {
	parset = list(...)
#	name2 = name
#	if (!missing(task)) {
#		if (is(task, "classif.task")) {
#			if (!extends(name, "wrapped.learner.classif")) {
#				name = paste(name, "classif", sep=".")
#				if (extends(name, "wrapped.learner.classif"))
#					name2 = name
#			} else {
#				name2 = name
#			}
#		} else if (is(task, "regr.task")){
#			if (!extends(name, "wrapped.learner.regr")) {
#				n = paste(name, "regr", sep=".")
#				if (extends(name, "wrapped.learner.regr"))
#					name2 = name
#			} else {
#				name2 = name
#			}
#		}
#	} else {
#		if (extends(name, "wrapped.learner"))
#			name2 = name
#	}
#	if (!is.null(name2))
		return(new(name, parset=parset))
#	else 
#		stop("Cannot find corresponding learner class for name: ", name)
}





