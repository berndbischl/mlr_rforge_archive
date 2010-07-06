



#' Generates a description object for a resampling strategy. 
#' 
#' @param method [string] \cr
#' 	      "cv" for cross-validation, "bs" for bootstrap, "subsample" for subsampling, "holdout" for holdout 	 			
#' @param iters [integer] \cr
#' 	      Number of resampling iterations. Not needed for "holdout". 	 			
#' @param ... [any] \cr
#'		Further parameters for strategies. 
#'      split: Percentage of training cases for hold-out / subsampling .
#' 
#' @return \code{\linkS4class{resample.desc}}.
#' @export 
#' @title Construct resampling description.



make.res.desc = function(method, iters, ...) {
	cc = paste(method, "desc", sep=".")
	if (!missing(iters)) {
		iters = as.integer(iters)
		return(new(cc, iters=iters, ...))
	} else {
		return(new(cc, ...))
	}
	
		
}
