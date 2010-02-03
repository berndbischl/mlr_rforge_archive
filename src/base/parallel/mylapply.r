mylapply <- function(xs, f, ...) {
	if (.mlr.local$parallel.setup$mode == "local") {
		ys <- lapply(xs, f, ...)
	} else {
		ys <- sfClusterApplyLB(xs, f, ...)
	}
	return(ys)
}


