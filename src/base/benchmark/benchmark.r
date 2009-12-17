#' @include task.learn.r
#' @include tune.wrapper.r

setGeneric(
		name = "benchmark",
		def = function(learner, task, resampling) {
			if (is.character(learner)) {
				learner = new(learner)
			}
			if (is(resampling, "resample.desc")) {
				resampling = make.resample.instance(resampling, nrow(task["data"]))
			}
			standardGeneric("benchmark")
		}
)

#' Conducts a benchmark experiment for a single classifier on a single
#' data set. This consists of an inner stage and outer stage. At the outer stage a 
#' tuning set and a test set are repeatedly formed from the data through resampling 
#' (usually cross-validation or bootstrapping). The respective hyperparameters of the 
#' classifier are tuned on the tuning set again through an inner resampling process,
#' the classifier is trained on the complete tuning set with the best found 
#' hyperparameters and the performance is measured on the test set. 
#'    
#' 
#' @param learner [\code{\linkS4class{wrapped.learner}} or \code{\link{character}}] \cr
#' 		  Learning algorithm.
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.
#' @param resampling [\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
#'        Resampling strategy. 
#'              
#' @return Data frame of tuning and test results. 
#' 
#' @export
#' @rdname benchmark
#' 
#' @usage benchmark(learner, task, resampling)
#' 
#' @examples
#' ct <- make.classif.task(data=iris, target="Species")
#' # very small grid for svm hyperpars 
#' r <- list(C=2^seq(-1,1), sigma=2^seq(-1,1))
#' inner.res <- make.cv.desc(iters=3)   
#' svm.tuner <- make.tune.wrapper("kernlab.svm.classif", method="grid", resampling=inner.res, control=grid.control(ranges=r))
#' res <- make.cv.instance(iters=5, size=nrow(iris))
#' benchmark(svm.tuner, ct, res)
#'
#' @title Benchmark experiment for one learner

setMethod(
		f = "benchmark",
		
		signature = c(learner="wrapped.learner", task="learn.task", resampling="resample.instance"),
		
		def = function(learner, task, resampling) {
			if (is(learner, "tune.wrapper")) {
				extract = function(x) {
					m = x["learner.model"]
					list(tuned.par=attr(m, "tuned.par"), tuned.perf=attr(m, "tuned.perf"))
				}
				rr <- resample.fit(learner, task, resampling, extract=extract)
				ex = rr@extracted
				ns = names(ex[[1]]$tuned.par)
				result = data.frame(matrix(nrow=resampling["iters"], ncol=length(ns)))
				colnames(result) = ns
				for (i in 1:length(ex)) {
					result[i, ns] = ex[[i]]$tuned.par
					result[i, "tune.perf"] = ex[[i]]$tuned.perf
				}
			} else {
				result = data.frame(matrix(nrow=resampling["iters"], ncol=0))
				rr <- resample.fit(learner, task, resampling)
			}
			rp <- resample.performance(task, rr)
			result[, "test.perf"] = rp$aggr2
			return(result)
		}
)


