#' Complete benchmark experiment for a learn task.
#' Allows you to compare a list of learning algorithms by measuring the test error w.r.t. to a given resampling strategy.  
#' Experiments are paired, meaning always the same training / test sets are used for the different learners.  
#' 
#' @param learners [\code{\link{list}} of \code{\linkS4class{wrapped.learner}} or \code{\link{character}}] \cr
#' 		  Defines the learning algorithms which should be compared.
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.
#' @param resampling [\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
#'        Resampling strategy. 
#' 
#' @return A matrix of test error. Columns correspond to learners, row to the iteration of the resampling strategy.
#' 
#' @usage bench.exp(learners, task, resampling)
#' 
#' @note You can also get automatic, internal tuning by using \code{\link{make.tune.wrapper}} with your learner. 
#' 
#' @seealso \code{\link{bench.add}}, \code{\link{make.tune.wrapper}} 
#' @export 
#' @aliases bench.exp 
#' @title Bencnmark experiment for multiple learners 
#'
#' @examples
#' ct <- make.classif.task(data=iris, target="Species")
#' # very small grid for svm hyperpars 
#' r <- list(C=2^seq(-1,1), sigma=2^seq(-1,1))
#' inner.res <- make.cv.desc(iters=3)   
#' svm.tuner <- make.tune.wrapper("kernlab.svm.classif", method="grid", resampling=inner.res, control=grid.control(ranges=r))
#' learners <- c("lda", "qda", svm.tuner)
#' res <- make.cv.instance(iters=5, size=nrow(iris))
#' bench.exp(learners, ct, res)
  

bench.exp <- function(learners, tasks, resampling) {
	if (!is.list(learners) && length(learners) == 1) {
		learners = list(learners)
	}
	if (!is.list(tasks)) {
		tasks = list(tasks)
	}
	learners = as.list(learners)
	n = length(learners)
	#bs = array(-1, nrow=resampling["iters"], ncol=n)
	## add dim for every loss ?? hmm, those are not always the same size...
	dims = c(resampling["iters"], n, 1, length(tasks))
	bs = array(dim = dims)
	
	learner.names = character()
	task.names = sapply(tasks, function(x) x["name"])	
	resamplings = list()
	tuned = list()
	cms = list()
	for (j in 1:length(tasks)) {
		task = tasks[[j]]
		if (is(resampling, "resample.desc")) {
			resamplings[[j]] = make.resample.instance(resampling, task["size"])
		}
		tuned[[j]] = as.list(rep(NA, n))
		cms[[j]] = as.list(rep(NA, n))
		for (i in 1:length(learners)) {
			wl = learners[[i]]
			if (is.character(wl))
				wl = make.learner(wl)
			learner.names[i] = wl["short.name"]
			bm = benchmark(wl, task, resamplings[[j]])
			bs[,i,1,j] = bm$result$test.perf
			if (is(wl, "tune.wrapper"))
				tuned[[j]][[i]] = bm$result
			if (is(task, "classif.task"))
				cms[[j]][[i]] = bm$conf
			else
				cms[[j]][[i]] = NA
		}
		names(tuned[[j]]) = learner.names
		names(cms[[j]]) = learner.names
		
	}
	dimnames(bs) = list(1:resampling["iters"], learner.names, NULL, task.names)
	names(tuned) = task.names
	names(cms) = task.names
	
	return(new("bench.result", perf = bs, tuned.pars=tuned, conf.mats=cms, resamplings=resamplings))
}