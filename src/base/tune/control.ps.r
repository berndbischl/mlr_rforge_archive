#' Control structure for pattern search tuning. 
#' 
#' @param start [numeric] \cr
#'    Named vector of initial values.
#' @param lower [numeric] \cr
#'    Named vector of lower boundary constraints. Default is -Inf.
#' @param upper [numeric] \cr
#'    Named vector of upper boundary constraints. Default is Inf.
#' @param delta [numeric] \cr
#'    Initial step size.   
#' @param delta.min [numeric] \cr
#'    Minimal step size. Default is 10^-3.   
#' @param maxit [integer] \cr
#'    Maximal number of iterations. Default is 100   
#' @param minimize [logical] \cr
#'       Minimize performance measure? Default is TRUE. 
#' @param scale [\code{\link{function}}] \cr
#'        A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a single, numerical vector and return a scaled one. Default is identity function.
#' 
#' @return Control structure for tuning.
#' @export 
#' @title Control for pattern search tuning.

ps.control <- function(
		start,
		lower=rep(-Inf, length(start)),	
		upper=rep(Inf, length(start)), 
		delta=1, 
		delta.min=10^(-3), 
		maxit=100,
		minimize=TRUE,
		scale=identity
) {
	x = list(start=unlist(start), lower=unlist(lower), upper=unlist(upper), delta=delta, delta.min=delta.min, maxit=maxit,
			minimize=minimize, scale=scale)
	class(x) = "ps.control"
	return(x)
}
