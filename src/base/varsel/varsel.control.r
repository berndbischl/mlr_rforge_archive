#' @export
varsel.control <- function(compare="diff", maxit=100, max.vars=Inf, 
	alpha=0.01, beta=0.01, gamma=NA, delta=NA, epsilon=NA) {
	list(compare=compare, max.vars=max.vars, minimize=TRUE, maxit=maxit,
	 	alpha=alpha, beta=beta, gamma=gamma, delta=delta, epsilon=epsilon 
	)
}

