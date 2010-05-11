#' Control structure for grid search tuning. 
#' 
#' @param ranges [\code{\link{list}}] \cr 
#' 		A list of named vectors/lists of possible values for each hyperparameter. 
#'      You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
#'      in the rare case when it does not make sense to search a complete cross-product of range values.     
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE. 
#' @param tune.threshold [logical] \cr 
#'       Perform empirical thresholding? Default is FALSE. Only supported for binary classification and you have to set type to "prob" for this in tune/make.tune.wrapper/bench.exp. 
#' @param scale [\code{\link{function}}] \cr 
#'        A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a vector and return a scaled one. Default is identity function.
#' 		    
#' @return Control structure for tuning.
#' @export 
#' @title Control for grid search tuning. 


grid.control <- function(ranges, minimize=TRUE, tune.threshold=FALSE, scale=identity) {
	new("tune.control", method="grid", ranges=ranges, minimize=minimize, tune.threshold=tune.threshold, scale=scale)
}