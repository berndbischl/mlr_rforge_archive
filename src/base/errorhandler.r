#' Sets up the errorhandler system of mlr. 
#' 
#' @param warn.on.par.without.desc [boolean] \cr
#'   Should a warning be generated if a parameter of a learner is set to a value, but no parameter description object exists, indicating possibly wrong name.
#' @return NULL.
#' @export
#' @title Errorhandler setup.

errorhandler.setup <- function(
		warn.on.par.without.desc=TRUE) {
	errrorhandler.setup = list()
	errrorhandler.setup$warn.on.pars.without.desc = warn.on.pars.without.desc
	
	.mlr.local$errrorhandler.setup <- errrorhandler.setup
	
	return(NULL)
}
