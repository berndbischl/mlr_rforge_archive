

warn.wrapper = function(x, f, ...) {
	assign(".mlr.slave.warnings", character(0), envir = .GlobalEnv)
	
	withCallingHandlers({
				y = f(x, ...)
			}, 
			warning = function(w) {
				sws = get(".mlr.slave.warnings", envir = .GlobalEnv) 
				assign(".mlr.slave.warnings", c(sws, w), envir = .GlobalEnv)
			}
	)
	sws = get(".mlr.slave.warnings", envir = .GlobalEnv) 
	if (length(sws) > 0)
		attr(y, ".mlr.slave.warnings") = sws
	return(y)
}


mylapply <- function(xs, f, from, ...) {
	ps = .mlr.local$parallel.setup
	if (ps$mode == "local" || ps$level != from) {
		y = lapply(xs, f, ...)
	} else if (ps$mode %in% c("sfCluster", "snowfall")){
		y = sfClusterApplyLB(x=xs, fun=warn.wrapper, f, ...)		
	} else if (ps$mode == "multicore") {
		# todo check warnings
		y = mclapply(xs, f, ..., mc.cores=ps$cpus)
	} else {
		stop("Unknown parallel model: ", ps$mode)
	}
	if (length(y) > 0) {
		for (i in 1:length(y)) {
			x = y[[i]]
			if (is(x, "try-error")) {
				stop(paste("On slave:", x))
			}
			ws = attr(x, ".mlr.slave.warnings")
			if (!is.null(ws)) {
				warning(paste("On slave:", ws))
				attr(y[[i]], ".mlr.slave.warnings") = NULL
			}
		}
	}
	return(y)
}



