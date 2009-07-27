
#' @importFrom utils assignInNamespace
#' @export 

parallel.setup <- function(mode="local", cpus=1, level="resample", global=FALSE) {
	p <- list()
	p$mode = mode
	p$level = level
	
	if (mode %in% c("sfCluster", "snowfall")) {
		if (mode == "sfCluster") {
			sfInit()
		} else if (mode == "snowfall") {
			sfStop()
			if (is.numeric(cpus))
				sfInit(parallel=T, cpus=cpus)
			else	
				sfInit(parallel=T, socketHosts=cpus)	
		} 
		sfLibrary(mlr)	
		ps <- getFromNamespace(".parallel.setup", "mlr")
		assign(".parallel.setup", ps, envir=.GlobalEnv)
		sfExport(".parallel.setup")
	}
	
	# i dont know why i need to do this. otherwise R CMD check wont accept the example code
	if (global)
		assign(".parallel.setup", p, envir=.GlobalEnv)
	else
		assignInNamespace(".parallel.setup", p, ns="mlr")
	return(p)
}



