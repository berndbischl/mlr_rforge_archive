
#' @export 

parallel.setup <- function(mode="local", cpus=1, level="resample", ...) {
	p <- list()
	p$mode = mode
	p$level = level
	p$cpus = cpus
	.mlr.local$parallel.setup <- p
	
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
		
		# we cannot export from package env
		# assign to global env
		assign(".mlr.local", .mlr.local, envir=.GlobalEnv)			
		# export and delete
		sfExport(".mlr.local")
		rm(.mlr.local, envir=.GlobalEnv)
		# set mode to local on slave so he does not parallelize 
		sfClusterEval(.mlr.local$parallel.setup$mode <- "local")
		# init random 
		sfClusterSetupRNG()
	} else if (mode == "multicore") {
		if(!require(multicore)) {
			stop("Please install the multicore package for this!")
			# todo set mode to local
		}
	} else if (!(mode %in% c("local", "multicore", "snowfall", "sfCluster"))) {
		.mlr.local$parallel.setup$mode <- "local"
		stop("Unknown parallel model: ", mode)
	}
}


