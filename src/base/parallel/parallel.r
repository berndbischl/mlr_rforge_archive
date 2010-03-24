
#' @export 

parallel.setup <- function(mode="local", cpus=1, level="resample", ...) {
	p <- list()
	p$mode = mode
	p$level = level
	p$cpus = cpus
	
	if (mode %in% c("sfCluster", "snowfall")) {
		if(!require(snowfall)) {
			stop("Please install the snowfall package for this!")				
		} 
		if (mode == "sfCluster") {
			sfInit(...)
		} else if (mode == "snowfall") {
			sfStop()
			sfSetMaxCPUs(cpus)
			if (is.numeric(cpus))
				sfInit(parallel=T, cpus=cpus, ...)
			else	
				sfInit(parallel=T, socketHosts=cpus, ...)
		} 
		# todo check version on nodes!
		x = sfClusterEval(require(mlr))
		if (!all(unlist(x)))
			stop("Could not load mlr on every node!")
		assign(".mlr.local.tmp" , .mlr.local, envir=.GlobalEnv)
						
		sfExport(".mlr.local.tmp")
		rm(.mlr.local.tmp,  envir=.GlobalEnv)

		sfClusterEval(init.slave())	
#		ps <- getFromNamespace(".parallel.setup", "mlr")
#		assign(".parallel.setup", ps, envir=.GlobalEnv)
		sfClusterSetupRNG()
	} else if (mode == "multicore") {
		if(!require(multicore)) {
			stop("Please install the multicore package for this!")				
		}
	}
	
	.mlr.local$parallel.setup <- p
	
	if (!(mode %in% c("local", "multicore", "snowfall", "sfCluster"))) {
		.mlr.local$parallel.setup$mode <- "local"
		stop("Unknown parallel model: ", mode)
	}
}


init.slave <- function() {
	.mlr.local$parallel.setup <- .mlr.local.tmp$parallel.setup 
	.mlr.local$logger.setup <- .mlr.local.tmp$logger.setup 
}


