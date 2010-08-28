

#' Defines the technical mode and level of parallelization when executing code.
#' 
#' @param mode [string] \cr
#' 		  Which parallel mode should be used: local, multicore, snowfall, sfCluster.
#' @param parallel.type [string] \cr
#'        Currently this is only used for sfInit, where it is passed to the 'type' argument. Default is 'MPI'. 
#' @param cpus [numeric] \cr
#'        Number of requested cpus. Default is mpi.universe.size() for snowfall/MPI, ignored for for sfCluster and 1 otherwise. 
#' @param level [string] \cr
#'      What is parallelized / what is a job. 
#' 		  resample: resample.fit is parallelized and a job is train / test.
#' 		  tune: tune is parallelized and a job is a resampled evaluation of one hyperparameter setting.  
#' 		  varsel: varsel is parallelized and a job is a resampled evaluation of a feature set.
#' 		  bench: bennch.exp is parallelized and a job is completely evaluating one learner on one data set.  
#' @param ... [any] \cr
#'        Optional parameters, only passed to sfInit currently. 
#' 
#' @export 
#' @title Parallelization setup. 
 

parallel.setup <- function(mode="local", parallel.type, cpus, level="resample", ...) {
	# check mode
	if (!(mode %in% c("local", "multicore", "snowfall", "sfCluster"))) {
		.mlr.local$parallel.setup$mode = "local"
		stop("Unknown parallel model: ", mode)
	}
  
  # check level
  if (!(level %in% c("resample", "tune", "varsel", "bench"))) {
    .mlr.local$parallel.setup$mode = "local"
    stop("Unknown parallel level: ", level)
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
		cpus = ifelse(mode=="snowfall" && parallel.type=="MPI", mpi.universe.size(), 1)
	}
		
	p <- list()
	p$mode = mode
	p$level = level
	p$cpus = cpus
	.mlr.local$parallel.setup <- p
	# todo: maybe keep the export hashes when just changing the level of parallization? 
	# delete export hash when we (re)start the cluster	
	rm(list=ls(envir=.mlr.export), envir=.mlr.export)
	
	if (mode %in% c("sfCluster", "snowfall")) {
		sfStop()
		if (mode == "sfCluster") {
			# sfcluster does not need setmaxcpus i think....
			sfInit(...)
		} else if (mode == "snowfall") {
			sfSetMaxCPUs(cpus)
			sfInit(parallel=TRUE, type=parallel.type, cpus=cpus,  ...)
		} 
		# todo check version on nodes!
		x = sfClusterEval(require(mlr))
		if (!all(unlist(x))) {
			.mlr.local$parallel.setup$mode = "local"
			stop("Could not load mlr on every node!")
		}
    # init slave with errorhandler, logger and set parallel=local
    sfClusterCall(function(x) mlr:::.mlr.set.local.on.slave(x), .mlr.local)
		# init random 
		sfClusterSetupRNG()
	}
}

.mlr.set.local.on.slave = function(mlrloc) {
  ls = mlrloc$logger.setup
  logger.setup(console=ls$console, file=ls$file, level=ls$global.level, sublevel=ls$sublevel)  
  eh = mlrloc$errorhandler.setup
  errorhandler.setup(on.learner.error=eh$on.learner.error, 
      on.par.without.desc=eh$on.par.without.desc, on.convert.var=eh$on.convert.var)
  ps = mlrloc$parallel.setup
  parallel.setup(mode="local")
}
