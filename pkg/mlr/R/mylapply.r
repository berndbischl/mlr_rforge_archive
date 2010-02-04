mylapply <- function(xs, f, from, ...) {
	ps = .mlr.local$parallel.setup
	if (ps$mode == "local" || ps$level != from) {
		ys <- lapply(xs, f, ...)
	} else {
		ys <- sfClusterApplyLB(xs, f, ...)
	}
	return(ys)
}


