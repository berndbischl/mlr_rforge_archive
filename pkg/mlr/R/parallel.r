
#' @export 

parallel.setup <- function(mode="local", cpus=1, level="resample") {
	p <- list()
	p$mode = mode
	p$level = level
	
	if (mode %in% c("sfCluster", "snowfall")) {
		if(!require(snowfall)) {
			stop("Please install the snowfall package for this!")				
		} 
		if (mode == "sfCluster") {
			sfInit()
		} else if (mode == "snowfall") {
			sfStop()
			if (is.numeric(cpus))
				sfInit(parallel=T, cpus=cpus)
			else	
				sfInit(parallel=T, socketHosts=cpus)
		} 
		sfClusterEval("require(mlr)")	
		assign(".mlr.local.tmp" , .mlr.local, envir=.GlobalEnv)
						
		sfExport(".mlr.local.tmp")
		rm(.mlr.local.tmp,  envir=.GlobalEnv)

		sfClusterEval("init.slave()")	
#		ps <- getFromNamespace(".parallel.setup", "mlr")
#		assign(".parallel.setup", ps, envir=.GlobalEnv)
	}
	
	.mlr.local$parallel.setup <- p
}


init.slave <- function() {
	.mlr.local$parallel.setup <- .mlr.local.tmp$parallel.setup 
	.mlr.local$logger.setup <- .mlr.local.tmp$parallel.setup 
}


