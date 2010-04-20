#' Control structure for grid search tuning. 
#' 
#' @param ranges [\code{\link{list}}] \cr 
#' 		A list of named vectors/lists of possible values for each hyperparameter. 
#'      You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
#'      in the rare case when it does not make sense to search a complete cross-product of range values.     
#' @param minimize [logical]
#'       Minimize performance measure? Default is TRUE. 
#' @param scale [\code{\link{function}}]
#'        A function to scale the hyperparamters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a single, numerical vector and return a scaled one. Default is identity function.
#' 		    
#' @return Control structure for tuning.
#' @export 
#' @title Control for grid search tuning. 

grid.control <- function(ranges, minimize=TRUE, scale=identity) {
	x = list(ranges=ranges, minimize=minimize, scale=scale)
	class(x) = "grid.control"
	return(x)
}