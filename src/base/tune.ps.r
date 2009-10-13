ps.wrapper <- function(f, start, lower, upper, control) {
	if (is.null(control))
		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper)
	else
		ps <- pattern.search(f=f, start=start, lower=lower, upper=upper, control=control)
	par <- as.list(ps$par)
	list(par=par, val=ps$val)
}