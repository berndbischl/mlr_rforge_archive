#' Sets up the errorhandler system of mlr. 
#' 
#' @param on.learner.error [string] \cr
#'   What should happen if an error in an underlying learning algorithm is caught: 'stop', 'warn' or quiet. Default is 'warn'. 
#' @param on.par.without.desc [string] \cr
#'   What should happen if a parameter of a learner is set to a value, but no parameter description object exists, indicating a possibly wrong name: 'stop', 'warn' or quiet. Default is 'warn'. 
#' @param on.convert.var [string] \cr
#'    What should happen if an a variable is converted during the creation of a \code{\linkS4class{learn.task}}: 'warn' or 'quiet'. Default is 'warn'.
#' 
#' on.learner.error: 'stop': R exception is generated. 'warn': Model \code{\linkS4class{learner.failure}} will be created, which predicts only NAs and a warning will be generated. 'quiet': Same as 'warn' but withou the warning.\cr  
#' on.par.without.desc: What should happen if a parameter of a learner is set to a value, but no parameter description object exists, indicating a possibly wrong name. 'stop': R exception is generated. 'warn': Warning, but parameter is still passed along to learner. 'quiet': Same as 'warn' but withou the warning.\cr  
#' on.convert.var: In some cases variables are converted during the creation of the learn.task. Currently, integers are converted to numerics, characters to factors and for classification, target levels without corresponding observation are dropped. 'warn': Warning on conversion. 'quiet': Conversion done quietly.\cr
#'  
#' @return NULL.
#' @export
#' @title Errorhandler setup.

errorhandler.setup <- function(
		on.learner.error="warn",
		on.par.without.desc="warn",
		on.convert.var="warn"
	) {
	if (!(on.learner.error %in% c("quiet", "warn", "stop")))
		stop("on.learner.error has to be 'quiet', 'warn' or 'stop', you used:", on.learner.error)
	if (!(on.par.without.desc %in% c("quiet", "warn", "stop")))
		stop("on.par.without.desc has to be 'quiet', 'warn' or 'stop', you used:", on.par.without.desc)
	if (!(on.convert.var %in% c("quiet", "warn")))
		stop("on.convert.var has to be 'quiet' or 'warn', you used:", on.convert.var)
	
	errorhandler.setup = list()
	errorhandler.setup$on.par.without.desc = on.par.without.desc
	errorhandler.setup$on.learner.error = on.learner.error
	errorhandler.setup$on.convert.var = on.convert.var
	
	.mlr.local$errorhandler.setup <- errorhandler.setup
	
	return(NULL)
}
