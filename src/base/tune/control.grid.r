#' Control structure for grid search tuning. 
#' 
#' @param ranges [\code{\link{list}}] \cr 
#' 		A list of named vectors/lists of possible values for each hyperparameter. 
#'      You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
#'      in the rare case when it does not make sense to search a complete cross-product of range values.     
#' 		    
#' @return Control structure for tuning.
#' @export 
#' @title Control for grid search tuning 

grid.control <- function(ranges) {
	list(ranges=ranges)	
}