
#' @export 

parallel.setup <- function(mode="local", parallel.type, cpus, level="resample", ...) {
	# check mode
	if (!(mode %in% c("local", "multicore", "snowfall", "sfCluster"))) {
		.mlr.local$parallel.setup$mode = "local"
		stop("Unknown parallel model: ", mode)
	}

	# parallel.type
	if (missing(parallel.type)) 
		parallel.type = switch(mode, snowfall="MPI", "MPI")
	
	# load packages
	packs = switch(mode, multicore="multicore", sfCluster="snowfall", 
			snowfall=c("snowfall", switch(parallel.type, MPI="Rmpi", c())))
	packs.ok = sapply(packs, function(x) require(x, character.only = TRUE))
	if(!all(packs.ok)) 
		stop("Please install the following packages: ", paste(packs[!packs.ok], collapse=" "))				
	
	# cpus
	if (missing(cpus)) {
		cpus = ifelse(mode=="snowfall" && parallel.type=="Rmpi", mpi.universe.size(), 1)
	}
		
	p <- list()
	p$mode = mode
	p$level = level
	p$cpus = cpus
	.mlr.local$parallel.setup <- p
	
	if (mode %in% c("sfCluster", "snowfall")) {
		sfStop()
		if (mode == "sfCluster") {
			# sfcluster does not need setmaxcpus i think....
			sfInit(...)
		} else if (mode == "snowfall") {
			sfSetMaxCPUs(cpus)
			sfInit(parallel=T, type=parallel.type, cpus=cpus,  ...)
		} 
		# todo check version on nodes!
		x = sfClusterEval(require(mlr))
		if (!all(unlist(x))) {
			.mlr.local$parallel.setup$mode = "local"
			stop("Could not load mlr on every node!")
		}
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
	}
}


