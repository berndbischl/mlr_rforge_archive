#todo: include complex example with repcv / b632+!
#' Generates a description object for a resampling strategy.
#' 
#' Repeated cross-validation: Use \dQuote{RepCV}. Then you have to set the aggregation function for your preferred performance measure to 
#'   \dQuote{testgroup.mean} via \code{\link{setAggregation}}.
#' B632 bootstrap: Use \dQuote{BS} for bootstrap and set predict to \dQuote{both}. 
#'   Then you have to set the aggregation function for your preferred performance measure to 
#'   \dQuote{b632} via \code{\link{setAggregation}}.
#' B632+ bootstrap: Use \dQuote{BS} for bootstrap and set predict to \dQuote{both}. 
#'   Then you have to set the aggregation function for your preferred performance measure to 
#'   \dQuote{b632plus} via \code{\link{setAggregation}}.
#' 
#' @title Construct resampling description.
#' @param method [\code{character(1)}] \cr
#'   \dQuote{CV} for cross-validation, \dQuote{LOO} for leave-one-out, \dQuote{StratCV} for stratified cross-validation, \dQuote{RepCV} for repeated cross-validation,\cr
#'   \dQuote{BS} for out-of-bag bootstrap, \dQuote{Subsample} for subsampling, \dQuote{Holdout} for holdout.	
#' @param iters [\code{integer(1)}] \cr
#'   Number of resampling iterations. Ignored for \dQuote{Holdout}. Default is 10.	 			
#' @param predict [character] \cr
#'   What to predict during resampling: \dQuote{train}, \dQuote{test} or \dQuote{both} sets. Default is \dQuote{test}.
#' @param ... [any] \cr
#'		Further parameters for strategies.\cr 
#'			split [\code{numeric(1)}]: Proportion of training cases for \dQuote{Holdout} and \dQuote{Subsample} from between 0 and 1. Default is 2/3.\cr
#'			reps [integer(1)]: Repeats for \dQuote{RepCV}. Here \code{iters = folds * reps}. Default is 2. \cr
#'			folds [integer(1)]: Folds in the repeated CV for \code{RepCV}. Here \code{iters = folds * reps}. Default is 5. 
#' @return \code{\linkS4class{ResampleDesc}}.
#' @export
makeResampleDesc = function(method, iters=10L, predict="test", ...) {
  checkArg(method, choices = c('CV', 'LOO', 'StratCV', 'RepCV', 'BS', 'Subsample', 'Holdout'))    
  iters = convertInteger(iters)
  checkArg(iters, "integer", len=1, na.ok=FALSE)    
  checkArg(predict, "character", choices=c("train", "test", "both"))    
  cc = paste(method, "Desc", sep="")
  d = new(cc, iters=iters, ...)
  d@predict = predict
  return(d)
}
