#' Complete benchmark experiment to compare different learning algorithms 
#' across one or more tasks w.r.t. a given resampling strategy.  
#' Experiments are paired, meaning always the same training / test sets are used for the different learners.  
 
#' @param learners [character vector | \code{\link{wrapped.learner} | list of the previous two] \cr
#' 		  Defines the learning algorithms which should be compared.
#' @param tasks [\code{\link{learn.task} | list of the previous] \cr
#'        Defines the tasks.
#' @param resampling [resampling desc | resampling instance | list of the previous two] \cr
#'        Defines the resampling strategies for the tasks.
#' @param measures [see \code{\link{measures}]
#'        Performance measures. 
#' @param type [character] \cr
#'        Classification: vector of "response" | "prob" | "decision", specifying the types to predict.
#'        Default is "response".
#' 		  Ignored for regression.	 
#' @return \code{\linkS4class{bench.result}}.
#' 
#' @usage bench.exp(learners, tasks, resampling, measures, type="response")
#' 
#' @note You can also get automatic, internal tuning by using \code{\link{make.tune.wrapper}} with your learner. 
#' 
#' @seealso \code{\link{bench.add}}, \code{\link{make.tune.wrapper}} 
#' @export 
#' @aliases bench.exp 
#' @title Benchmark experiment for multiple learners and tasks. 



bench.exp <- function(learners, tasks, resampling, measures, type="response", 
		conf.mats=TRUE, predictions=FALSE, models=FALSE, 
		opt.pars=TRUE, opt.paths=FALSE)  {
	
	if (!is.list(learners) && length(learners) == 1) {
		learners = list(learners)
	}
	if (!is.list(tasks)) {
		tasks = list(tasks)
	}
	learners = as.list(learners)
	n = length(learners)
	
	if (missing(measures))
		measures = default.measures(tasks[[1]])
	measures = make.measures(measures)
	
	
	
	#bs = array(-1, nrow=resampling["iters"], ncol=n)
	## add dim for every loss ?? hmm, those are not always the same size...
	if (length(tasks) > 1 && is(resampling, "resample.instance")) {
		stop("Cannot pass a resample.instance with more than 1 task. Use a resample.desc!")
	}
	dims = c(resampling["iters"]+1, n, length(measures))
	bs = list()
	
	learner.names = character()
	task.names = sapply(tasks, function(x) x["name"])	
	resamplings = list()
	tds = dds = rfs = cms = mods = list()
	o.pars = o.paths = list()
	
	for (j in 1:length(tasks)) {
		bs[[j]] = array(0, dim = dims)		
		task = tasks[[j]]
		rfs[[j]] = list()
		cms[[j]] = list()
		mods[[j]] = list()
		o.pars[[j]] = list()
		o.paths[[j]] = list()
		if (is(resampling, "resample.desc")) {
			resamplings[[j]] = new(resampling@instance.class, resampling, task["size"])
		} else {
			resamplings[[j]] = resampling
		}		
		tds[[j]] = task@task.desc
		dds[[j]] = task@data.desc
		for (i in 1:length(learners)) {
			wl = learners[[i]]
			if (is.character(wl))
				wl = make.learner(wl)
			learner.names[i] = wl["short.name"]
			bm = benchmark(learner=wl, task=task, resampling=resamplings[[j]], measures=measures, type=type, models=models,
				opt.pars = opt.pars, opt.paths=opt.paths)
			rr = bm$result
			rf = bm$resample.fit
			# remove tune perf
			rr = rr[, names(measures)]
			bs[[j]][,i,] = as.matrix(rr)
			
			if(predictions)	rfs[[j]][[i]] = rf else	rfs[[j]][i] = list(NULL)
			if(is(task, "classif.task") && conf.mats) cms[[j]][[i]] = bm$conf else cms[[j]][i] = list(NULL)
			if(models)	mods[[j]][[i]] = bm$models else	mods[[j]][i] = list(NULL)
			if(opt.pars && is(wl, "tune.wrapper")) o.pars[[j]][[i]] = bm$opt.pars else o.pars[[j]][i] = list(NULL)
			if(opt.paths && is(wl, "tune.wrapper")) o.paths[[j]][[i]] = bm$opt.paths else o.paths[[j]][i] = list(NULL)
		}
		dimnames(bs[[j]]) = list(c(1:resampling["iters"], "combine"), learner.names, names(measures))

		names(rfs[[j]]) = learner.names
		names(cms[[j]]) = learner.names
		names(mods[[j]]) = learner.names
		names(o.pars[[j]]) = learner.names
		names(o.paths[[j]]) = learner.names
	}
	names(bs) = task.names
	names(rfs) = task.names
	names(cms) = task.names
	names(mods) = task.names
	names(o.pars) = task.names
	names(o.paths) = task.names
	return(new("bench.result", task.descs=tds, data.descs=dds, resamplings=resamplings, perf = bs, 
					predictions=rfs, conf.mats=cms, models=mods,
					opt.pars = o.pars, opt.paths = o.paths
	))
}