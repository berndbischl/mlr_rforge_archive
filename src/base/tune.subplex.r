subplex.wrapper <- function(f, start, lower, upper, control) {
	if (is.null(control))
		sp <- subplex(fn=f, par=start)
	else
		sp <- subplex(fn=f, par=start, control=control)
	par <- as.list(sp$par)
	list(par=par, val=sp$val)	
}