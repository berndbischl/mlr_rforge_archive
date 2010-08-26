#' Complete benchmark experiment to compare different learning algorithms 
#' across one or more tasks w.r.t. a given resampling strategy.  
#' Experiments are paired, meaning always the same training / test sets are used for the different learners.  
 
#' @param learners [string | \code{\linkS4class{learner}} | list of the previous two] \cr
#' 		  Defines the learning algorithms which should be compared.
#' @param tasks [\code{\link{learn.task}} | list of the previous] \cr
#'        Defines the tasks.
#' @param resampling [resampling desc | resampling instance | list of the previous two] \cr
#'        Defines the resampling strategies for the tasks.
#' @param measures [see \code{\link{measures}}]
#'        Performance measures. 
#' @param conf.mats [logical] \cr
#'        Should confusion matrices be stored?
#'        Default is TRUE.
#' 		  Ignored for regression.	 
#' @param predictions [logical] \cr
#'        Should all predictions be stored?
#'        Default is FALSE.
#' @param models [logical] \cr
#'        Should all fitted models be stored?
#'        Default is FALSE.
#' @param paths [logical] \cr
#'        Should the optimization paths be stored?
#'        Default is FALSE.
#' @return \code{\linkS4class{bench.result}}.
#' 
#' @usage bench.exp(learners, tasks, resampling, measures, conf.mats=TRUE, predictions=FALSE, models=FALSE, paths=FALSE)
#' 
#' @note You can also get automatic, internal tuning by using \code{\link{make.tune.wrapper}} with your learner. 
#' 
#' @seealso \code{\link{make.tune.wrapper}} 
#' @export 
#' @aliases bench.exp 
#' @title Benchmark experiment for multiple learners and tasks. 


# todo: check unique ids
bench.exp <- function(learners, tasks, resampling, measures,  
		conf.mats=TRUE, predictions=FALSE, models=FALSE, paths=FALSE)  {
	
	if (!is.list(learners) && length(learners) == 1) {
		learners = list(learners)
	}
	learners = as.list(learners)
	if (length(learners) == 0)
		stop("No learners were passed!")
	check.list.type(learners, c("character", "learner"))
	learners = lapply(learners, function(x) if (is.character(x)) make.learner(x) else x)
	ids = sapply(learners, function(x) x["id"])
	if (any(duplicated(ids)))
		stop("Learners need unique ids!")
	
	if (!is.list(tasks)) {
		tasks = list(tasks)
	}
	if (length(tasks) == 0)
		stop("No tasks were passed!")
	check.list.type(tasks, "learn.task")
	
	if (missing(measures))
		measures = default.measures(tasks[[1]])
	measures = make.measures(measures)
	
	
	
	#bs = array(-1, nrow=resampling["iters"], ncol=n)
	## add dim for every loss ?? hmm, those are not always the same size...
	if (length(tasks) > 1 && is(resampling, "resample.instance")) {
		stop("Cannot pass a resample.instance with more than 1 task. Use a resample.desc!")
	}
	bs = list()
	
	learner.names = character()
	task.names = sapply(tasks, function(x) x["id"])	
	resamplings = list()
	tds = dds = rfs = cms = mods = list()
	ors = list()
	
	# function do apply all learners to one task
	be.task = function(task, learners, resampling, measures, conf.mats, models, paths) {
		dims = c(resampling["iters"]+1, length(learners), length(measures))
		bs = array(0, dim = dims)		
		logger.info("bench.exp: task = ", task["id"])
		rfs = list()
		cms = list()
		mods = list()
		ors = list()
		if (is(resampling, "resample.desc")) {
			res = make.res.instance(resampling, task=task)
		} else {
			res = resampling
		}		
		for (i in 1:length(learners)) {
			wl = learners[[i]]
			learner.names[i] = wl["id"]
			logger.info("bench.exp: learner = ", wl["id"])
			bm = benchmark(learner=wl, task=task, resampling=res, measures=measures, conf.mat=conf.mats, models=models, paths=paths)
			rr = bm$result
			rf = bm$resample.fit
			# remove tune perf
			rr = rr[, names(measures)]
			bs[,i,] = as.matrix(rr)
			
			if(predictions)	rfs[[i]] = rf else	rfs[i] = list(NULL)
			if(is(task, "classif.task") && conf.mats) cms[[i]] = bm$conf else cms[i] = list(NULL)
			if(models)	mods[[i]] = bm$models else	mods[i] = list(NULL)
			if(is(wl, "opt.wrapper")) ors[[i]] = bm$ors else ors[i] = list(NULL)
		}
		dimnames(bs) = list(c(1:res["iters"], "combine"), learner.names, names(measures))
		
		names(rfs) = learner.names
		names(cms) = learner.names
		names(mods) = learner.names
		names(ors) = learner.names
		return(list(bs=bs, resampling=res, rfs=rfs, cms=cms, mods=mods, ors=ors))		
	}
	
	results = lapply(tasks, function(task) 
				be.task(task, learners, resampling, measures, conf.mats, models, paths))
	names(results) = task.names
	
	resamplings = lapply(results, function(x) x$resampling)
	bs  = lapply(results, function(x) x$bs)
	rfs  = lapply(results, function(x) x$rfs)
	cms  = lapply(results, function(x) x$cms)
	mods = lapply(results, function(x) x$mods)
	ors  = lapply(results, function(x) x$ors)
	
	tds = lapply(tasks, function(x) x@task.desc); names(tds) = task.names
	dds = lapply(tasks, function(x) x@data.desc); names(dds) = task.names
	
	return(new("bench.result", task.descs=tds, data.descs=dds, resamplings=resamplings, perf = bs, 
					predictions=rfs, conf.mats=cms, models=mods,
					opt.results = ors
	))
}