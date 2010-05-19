#' Control structure for CMA-ES tuning. 
#' 
#' See \code{\link[cmaes]{cma_es}} for details of CMA-ES settings.
#' 
#' @param start [numeric] \cr
#'    Named vector of initial values.
#' @param minimize [logical] \cr
#'       Minimize performance measure? Default is TRUE. 
#' @param scale [\code{\link{function}}] \cr
#'        A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a single, numerical vector and return a scaled one. Default is identity function.
#' @param ... Passed down to cmaes.
#' 
#' @return Control structure for tuning.
#' @export 
#' @title Control for CMA-ES tuning.

cmaes.control <- function(start, minimize=T, scale=identity, ...) {
	x = list(...)
	y = list(start=unlist(start), minimize=minimize, scale=scale)
	y = c(y, x)
	class(y) = "cmaes.control"
	return(y)
}
