cmaes.wrapper <- function(f, start, lower, upper, control) {
	res <- cma.es(par=start, fn=f, lower=lower, upper=upper, control=control)
	par <- as.list(res$par)
	list(par=par, val=res$val)
}