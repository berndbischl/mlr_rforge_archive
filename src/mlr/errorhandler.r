#' Sets up the error handling system of mlr.  
#' 
#' @param on.learner.error [character(1)] \cr
#' What should happen if an error in an underlying learning algorithm is caught: 'stop', 'warn' or 'quiet'.\cr 
#' 'stop': R exception is generated.\cr
#' 'warn': Model \code{\linkS4class{learner.failure}} will be created, which predicts only NAs and a warning will be generated.\cr 
#' 'quiet': Same as 'warn' but withou the warning.\cr
#' Default is 'warn'. 
#' @param on.par.without.desc [character(1)] \cr
#' What should happen if a parameter of a learner is set to a value, but no parameter description object exists, indicating a possibly wrong name: 'stop', 'warn' or quiet.\cr
#' 'stop': R exception is generated.\cr
#' 'warn': Warning, but parameter is still passed along to learner.\cr 
#' 'quiet': Same as 'warn' but without the warning.\cr
#' Default is 'warn'. 
#' @param on.convert.var [character(1)] \cr
#'   What should happen if an a variable is converted during the creation of a \code{\linkS4class{LearnTask}}: 'warn' or 'quiet'.\cr 
#'   Currently, integers are converted to doubles, characters to factors and for classification, target levels without corresponding observation are dropped.\cr 
#'   'warn': Warning on conversion.\cr
#'   'quiet': Conversion done quietly.\cr
#'   Default is 'warn'.
#' @param on.convert.varname [character(1)] \cr
#'   What should happen if an a variable is renamed during the creation of a \code{\linkS4class{LearnTask}}: 'stop', 'warn' or 'quiet'.\cr 
#'   The following special characters are converted to their UTF8 integer codes, because they might create problems
#'   later on in a learner if they occur in a feature name: [ ] ( ) { } , + - * / =  ~  
#'   'stop': R exception is generated.\cr
#'   'warn': Warning on rename.\cr
#'   'quiet': Renaming done quietly.\cr
#'   Default is 'warn'.
#'  
#' @return NULL.
#' @export
#' @title Errorhandler setup.

errorhandler.setup <- function(
		on.learner.error="warn",
		on.par.without.desc="stop",
		on.convert.var="warn",
    on.convert.varname="warn"
  ) {
	if (!(on.learner.error %in% c("quiet", "warn", "stop")))
		stop("on.learner.error has to be 'quiet', 'warn' or 'stop', you used:", on.learner.error)
	if (!(on.par.without.desc %in% c("quiet", "warn", "stop")))
		stop("on.par.without.desc has to be 'quiet', 'warn' or 'stop', you used:", on.par.without.desc)
	if (!(on.convert.var %in% c("quiet", "warn")))
		stop("on.convert.var has to be 'quiet' or 'warn', you used:", on.convert.var)
  if (!(on.convert.varname %in% c("stop", "quiet", "warn")))
    stop("on.convert.varname has to be 'quiet', 'warn' or 'stop', you used:", on.convert.varname)
  
	errorhandler.setup = list()
	errorhandler.setup$on.par.without.desc = on.par.without.desc
	errorhandler.setup$on.learner.error = on.learner.error
	errorhandler.setup$on.convert.var = on.convert.var
  errorhandler.setup$on.convert.varname = on.convert.varname
  
	.mlr.local$errorhandler.setup <- errorhandler.setup
	
	return(NULL)
}
