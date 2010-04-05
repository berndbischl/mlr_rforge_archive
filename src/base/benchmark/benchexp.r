#' Complete benchmark experiment to compare different learning algorithms 
#' accros one or more tasks w.r.t. to a given resampling strategy.  
#' Experiments are paired, meaning always the same training / test sets are used for the different learners.  
 
#' @param learners [character vector | wrapped.learner | list of the previous two] \cr
#' 		  Defines the learning algorithms which should be compared.
#' @param tasks [learn.task | list of the previous] \cr
#'        Defines the tasks.
#' @param measures [see measures]
#'        Performance measures. 
#' @param type [string] \cr
#'        Classification: "response" | "prob" | "decision" 
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



bench.exp <- function(learners, tasks, resampling, measures, type="response") {
	
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
	dims = c(resampling["iters"]+1, n, length(measures), length(tasks))
	bs = array(0, dim = dims)
	
	learner.names = character()
	task.names = sapply(tasks, function(x) x["name"])	
	resamplings = list()
	tuned = list()
	cms = list()
	rfs = list()
	tds = list()
	dds = list()
	for (j in 1:length(tasks)) {
		task = tasks[[j]]
		if (is(resampling, "resample.desc")) {
			resamplings[[j]] = new(resampling@instance.class, resampling, task["size"])
		} else {
			resamplings[[j]] = resampling
		}		
		tuned[[j]] = as.list(rep(NA, n))
		cms[[j]] = as.list(rep(NA, n))
		rfs[[j]] = as.list(rep(NA, n))
		tds[[j]] = task@task.desc
		dds[[j]] = task@data.desc
		for (i in 1:length(learners)) {
			wl = learners[[i]]
			if (is.character(wl))
				wl = make.learner(wl, task)
			learner.names[i] = wl["short.name"]
			bm = benchmark(learner=wl, task=task, resampling=resamplings[[j]], measures=measures, type=type)
			rr = bm$result
			# remove tune perf
			rr = rr[, names(measures)]
			bs[,i,,j] = as.matrix(rr)
			if (is(wl, "tune.wrapper"))
				tuned[[j]][[i]] = bm$result
			if (is(task, "classif.task"))
				cms[[j]][[i]] = bm$conf
			else
				cms[[j]][[i]] = NA
			rfs[[j]][[i]] = bm$resample.fit
		}
		names(tuned[[j]]) = learner.names
		names(cms[[j]]) = learner.names
		names(rfs[[j]]) = learner.names
	}
	dimnames(bs) = list(c(1:resampling["iters"], "combine"), learner.names, names(measures), task.names)
	names(tuned) = task.names
	names(cms) = task.names
	return(new("bench.result", task.descs=tds, data.descs=dds, resamplings=resamplings, perf = bs, tuned.pars=tuned, conf.mats=cms, resample.fits=rfs))
}