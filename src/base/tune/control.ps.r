#' Control structure for pattern search tuning. 
#' 
#' @param start [\code{\link{numeric}}] \cr
#'    Named vector for initial values.
#' @param lower [\code{\link{numeric}}] \cr
#'    Named vector for lower boundary constraints. Default is -Inf.
#' @param upper [\code{\link{numeric}}] \cr
#'    Named vector for upper boundary constraints. Default is -Inf.
#' @param delta [\code{\link{numeric}}] \cr
#'    Initial step size.   
#' @param delta [\code{\link{numeric}}] \cr
#'    Minimal step size. Default is 10^-3.   
#' @param maxit [\code{\link{integer}}] \cr
#'    Maximal number of iterations. Default is 100   

#' @return Control structure for tuning.
#' @export 
#' @title Control for pattern search tuning 

ps.control <- function(
		start,
		lower=rep(-Inf, length(start)),	
		upper=rep(Inf, length(start)), 
		delta=1, 
		delta.min=10^(-3), 
		maxit=100) {
	list(start=unlist(start), lower=unlist(lower), upper=unlist(upper), delta=delta, delta.min=delta.min, maxit=maxit)	
}
