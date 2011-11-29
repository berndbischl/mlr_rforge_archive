#' Defines the technical mode and level of parallelization when executing code.
#'
#' Note that when you parallelize a function, e.g. \code{\link{resample}}, this also works when the function is internally called by 
#' mlr in a more complex function, e.g. \code{\link[mlrBenchmark]{benchmark}}. 
#' 
#' @title Parallelization setup. 
#' @param mode [\code{character(1)}]\cr
#'   Which parallel mode should be used: 
#'   \dQuote{local}, \dQuote{multicore}, \dQuote{snowfall}.
#'   Default is \dQuote{local} without parallel execution.
#' @param cpus [\code{integer(1)}]\cr
#'   Number of requested cpus. Default is \code{\link[Rmpi]{mpi.universe.size}} for snowfall/MPI and 1 otherwise. 
#' @param level [\code{character(1)}]\cr
#'   What is parallelized / what is a job:\cr
#' 	 \dQuote{resample}: \code{\link{resample}} is parallelized and a job is train / test.\cr
#'   \dQuote{opt}: \code{\link[mlrTune]{tune}} and \code{\link[mlrVarsel]{varsel}} are parallelized and a job is a resampled evaluation of one hyperparameter setting/feature set.\cr  
#'   \dQuote{bench}: \code{\link[mlrBenchmark]{benchmark}} is parallelized and a job is completely evaluating one learner on one data set.
#' @param ... [any]\cr
#'    Optional parameters, only passed to \code{\link[snowfall]{sfInit}} currently. 
#' @return Nothing.
#' @export 

setupParallel = function(mode="local", cpus, level="resample", ...) {
  checkArg(mode, choices=c("local", "multicore", "snowfall"))
  type = coalesce(..., "none")
  packs = 
    if (mode == "multicore") 
      "multicore"
    else if (mode == "snowfall") 
      if (type == "MPI") c("snowfall", "Rmpi") else "snowfall"
    else
      character(0)
  requirePackages(packs)
	if (missing(cpus))
		cpus = if(mode=="snowfall" && type=="MPI") mpi.universe.size() else 1L
  checkArg(cpus, "integer", len=1, na.ok=FALSE)
  checkArg(level, choices=c("resample", "opt", "bench"))
		
	
  .mlr.local$parallel.setup = list(
    mode = mode,
    level = level,
    cpus = cpus
  )
	
	# todo: maybe keep the export hashes when just changing the level of parallization? 
	# delete export hash when we (re)start the cluster	
	rm(list=ls(envir=.mlr.export), envir=.mlr.export)
	
	if (mode == "snowfall") {
		sfStop()
   	sfSetMaxCPUs(cpus)
		sfInit(parallel=TRUE, cpus=cpus, ...)
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
  invisible(NULL)
}

# todo:wipe global envir on slave

.mlr.set.local.on.slave = function(conf) {
  ls = conf$logger.setup
  setupLogger(level=ls$global.level, show.learner.output=ls$show.learner.output)  
  eh = conf$errorhandler.setup
  setupErrorHandler(on.learner.error=eh$on.learner.error, on.par.without.desc=eh$on.par.without.desc)
  ps = conf$parallel.setup
  setupParallel(mode="local")
}
