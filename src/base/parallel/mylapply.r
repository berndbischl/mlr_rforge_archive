mylapply <- function(xs, f, from, ...) {
	ps = .mlr.local$parallel.setup
	if (ps$mode == "local" || ps$level != from) {
		y = lapply(xs, f, ...)
	} else if (ps$mode %in% c("sfCluster", "snowfall")){
		y = sfClusterApplyLB(xs, f, ...)
	} else if (ps$mode == "multicore") {
		y = mclapply(xs, f, ..., mc.cores=ps$cpus)
	} else {
		stop("Unknown parallel model: ", ps$mode)
	}
	return(y)
}



