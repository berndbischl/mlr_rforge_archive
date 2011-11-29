#' Sets up the error handling system of mlr.  
#' 
#' @title Errorhandler setup.
#' @param on.learner.error [\code{character(1)}]\cr
#'   What should happen if an error in an underlying learning algorithm is caught:\cr 
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: A 'failure model' will be created, which predicts only NAs and a warning will be generated.\cr 
#'   \dQuote{quiet}: Same as 'warn' but withou the warning.\cr
#' Default is \dQuote{warn}. 
#' @param on.par.without.desc [\code{character(1)}]\cr
#'   What should happen if a parameter of a learner is set to a value, but no parameter description object exists, indicating a possibly wrong name: 'stop', 'warn' or quiet.\cr
#'   \dQuote{stop}: R exception is generated.\cr
#'   \dQuote{warn}: Warning, but parameter is still passed along to learner.\cr 
#'   \dQuote{quiet}: Same as \dQuote{warn} but without the warning.\cr
#'   Default is \dQuote{warn}. 
#' @return Nothing.
#' @export

setupErrorHandler = function(on.learner.error="warn", on.par.without.desc="stop") {
  checkArg(on.learner.error, choices=c("quiet", "warn", "stop"))
  checkArg(on.par.without.desc, choices= c("quiet", "warn", "stop"))
    
	.mlr.local$errorhandler.setup = list(
    on.par.without.desc = on.par.without.desc,
    on.learner.error = on.learner.error
  )  
	invisible(NULL)
}
