
#' Control structure for Nelder-Mead tuning. 
#' 
#' See optim for details of Nelder-Mead settings.
#' 
#' @param start [numeric] \cr
#'    Named vector of initial values.
#' @param minimize [logical]
#'       Minimize performance measure? Default is TRUE. 
#' @param scale [\code{\link{function}}]
#'        A function to scale the hyperparamters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a single, numerical vector and return a scaled one. Default is identity function.
#' @param ... Passed down to optim.
#' 
#' @return Control structure for tuning.
#' @export 
#' @title Control for Nelder-Mead tuning.


nm.control <- function(start, minimize=T, scale=identity, ...) {
	x = list(...)
	y = list(start=unlist(start), minimize=minimize, scale=scale)
	y = c(y, x)
	class(y) = "nm.control"
	return(y)
}
