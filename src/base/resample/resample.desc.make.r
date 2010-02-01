



#' Generates a description object for a resampling strategy. 
#' 
#' @param size [\code{\link{integer}}] \cr
#'        Size of the data set to resample.
#' @param ... Futher parameters for strategies.\cr 
#'        iters: Number of generated subsets / resampling iterations.
#'        split: Percentage of training cases for hold-out / subsampling .
#' 
#' @return A \code{\linkS4class{subsample.desc}} object.
#' @export 
#' @title Construct subsampling description



make.res.desc = function(method, ...) {
	cc <- paste(method, "desc", sep=".")
	return(new(cc, ...))
}
