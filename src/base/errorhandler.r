#' Sets up the errorhandler system of mlr. 
#' 
#' @param warn.on.par.without.desc [boolean] \cr
#'   Should a warning be generated if a parameter of a learner is set to a value, but no parameter description object exists, indicating possibly wrong name. Default is TRUE. 
#' @param stop.on.learner.error [boolean] \cr
#'   Should errors in the underlying learner algorithm be caught. If this argument is FALSE, in case of an error an internal \code{\linkS4class{learner.failure}} will be created, which predicts only NAs. Default is FALSE. 
#' @return NULL.
#' @export
#' @title Errorhandler setup.

errorhandler.setup <- function(
		warn.on.par.without.desc=TRUE,
		stop.on.learner.error=FALSE
	) {
	errorhandler.setup = list()
	errorhandler.setup$warn.on.par.without.desc = warn.on.par.without.desc
	errorhandler.setup$stop.on.learner.error = stop.on.learner.error
	
	.mlr.local$errorhandler.setup <- errorhandler.setup
	
	return(NULL)
}
