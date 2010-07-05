



#' Generates a description object for a resampling strategy. 
#' 
#' @param method [string] \cr
#' 	      "cv" for cross-validation, "bs" for bootstrap, "subsample" for subsampling, "holdout" for holdout 	 			
#' @param ... [any] \cr
#'		Further parameters for strategies. 
#'      iters: Number of resampling iterations.
#'      split: Percentage of training cases for hold-out / subsampling .
#' 
#' @return \code{\linkS4class{resample.desc}}.
#' @export 
#' @title Construct resampling description.



make.res.desc = function(method, iters, ...) {
	cc = paste(method, "desc", sep=".")
	iters = as.integer(iters)
	return(new(cc, iters=iters, ...))
}
