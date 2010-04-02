varsel.control <- function(compare="diff", alpha=0.01, beta=0.01, max.vars=Inf, maxit=100) {
	list(compare=compare, alpha=alpha, beta=beta, max.vars=max.vars, minimize=TRUE, maxit=maxit)
}

