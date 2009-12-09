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

#' \code{benchmark} conducts a benchmark experiment for a single classifier on a single
#' data set. This consists of an inner stage and outer stage. At the outer stage a 
#' tuning set and a test set are repeatedly formed from the data through resampling 
#' (usually cross-validation or bootstrapping). The respective hyperparameters of the 
#' classifier are tuned on the tuning set again through an inner resampling process,
#' the classifier is trained on the complete tuning set with the best found 
#' hyperparameters and the performance is measured on the test set. 
#'    
#' 
#' @param learn.task [\code{\linkS4class{learn.task}}] \cr
#'    Learning task.   
#' @param 	ranges [\code{\link{list}}] \cr 
#'    List of named range vectors/list for hyperparameters used in tuning 
#' 	  (see \code{\link{tune}}).
#' @param measure [\code{\link{character}}/\code{\link{list}}] \cr 
#'    Name of performance measure to optimize or a list describing your own performance measure. 
#' 	  The default is mean misclassification error. 
#' @param outer.resampling [\code{\link{resample.instance}}] \cr 
#'   	Specifies the training and test indices of the resampled data used in outer stage. 
#' @param inner.resampling [\code{\link{resample.desc}}] \cr 
#'    Describes resampling method to be used in inner stage. 
#' @param 	all.tune.results [\code{\link{logical}}] \cr 
#' 	  Should complete results for all inner tunings be returned? (default is FALSE)
#'              
#' @return If \code{all.tune.results} is FALSE (default) benchmark returns a list 
#'  containing the best parameter combinations, their inner run mean performance, 
#'  the standard deviation of their inner run performance and their test performance.
#'  If \code{all.tune.results} is TRUE the output contains additional information about all
#'  tested parameters by the inner runs.
#' 
#' @export
#' @rdname benchmark
#' 
#' @usage benchmark(learner, task, resampling)
#' 
#' @examples
#' # set up the learning task and parameter grid
#' ct <- make.classif.task("kernlab.svm.classif", data=iris, target="Species")
#' ranges <- list(kernel="polydot", degree=1:3, C=2^seq(-2,2))
#' # create the outer cross-validation
#' or <- make.cv.instance(iters=5, size=nrow(iris))					
#' # describe the inner cross-validation
#' ir <- make.cv.desc(iters=3)
#' benchmark(learn.task=ct, ranges=ranges, outer.resampling=or, inner.resampling=ir)
#'
#' @title benchmark

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


