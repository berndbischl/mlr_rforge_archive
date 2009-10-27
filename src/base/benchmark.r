#' @include tune.r
roxygen()


setGeneric(
		name = "benchmark",
		def = function(learn.task, outer.resampling, inner.resampling, ranges, measure, all.tune.results) {
			if (missing(ranges))
				ranges <- list()
			if (missing(inner.resampling))
				inner.resampling <- new("cv.desc", folds=5)
			if (missing(measure))
				measure <- make.default.measure(learn.task)
			if (missing(all.tune.results))
				all.tune.results <- FALSE
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
#' @usage benchmark(learn.task, outer.resampling, inner.resampling, ranges, measure, all.tune.results)
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
		
		signature = c(learn.task="learn.task", outer.resampling="resample.instance", inner.resampling = "resample.desc", 
				ranges="list", measure="list", all.tune.results="logical"),
		
		def = function(learn.task, outer.resampling, inner.resampling, ranges, measure, all.tune.results) {
			if (length(ranges) == 0) {
				logger.debug("Ranges empty, so just normal resample.fit.")
				rr <- resample.fit(learn.task=learn.task, resample.instance=outer.resampling)
				rp <- resample.performance(learn.task, outer.resampling, rr, measure=measure)
				return(data.frame(test.perf=rp$values))
			}
			
			if (all(sapply(ranges, is.list))) {
				logger.debug("Ranges is list of lists, call benchmark.1 repeatedly.")
				tune.results = NULL
				bs <- lapply(ranges, function(r) benchmark.1(learn.task, outer.resampling, inner.resampling, r, measure, all.tune.results))
				
				if (!all.tune.results)
					fun <- function(a,b) rbind.fill(a,b)
				else 
					fun <- function(a,b) list(performances=rbind.fill(a$perf, b$perf), c(a$tune, b$tune))
				result <- Reduce(fun, bs)
				return(result)
			}
			return(benchmark.1(learn.task, outer.resampling, inner.resampling, ranges, measure, all.tune.results))			
		}
)


benchmark.1 <- function(learn.task, outer.resampling, inner.resampling, ranges, measure, all.tune.results) {
	
	
	rin <- outer.resampling
	tune.result <- list()
	# make empty df with cols the same as the names of the ranges
	ns <- names(ranges)
	result <- data.frame(matrix(nrow=0,ncol=length(ns)))
	colnames(result) <- ns
	
	for (i in 1:rin["iters"]) {
		train.i <- rin["train.inds", i]
		test.i <- rin["test.inds", i]
		ct2 <- restrict.learn.task(learn.task, train.i)
		ir = make.resample.instance(desc=inner.resampling, size=length(train.i))            
		tune.result[[i]] <- tune(ct2, ir, ranges, measure)
		best.pars <- tune.result[[i]]$best.parameters
		
		result[i,names(best.pars)] <- best.pars     
		
		
		result[i, "tune.perf.aggr"] <- tune.result[[i]]$best.performance
		result[i, "tune.perf.spread"] <- tune.result[[i]]$best.spread
		
		
		# there might be NAs if tune was called with a list of list of ranges
		best.pars2 <- best.pars[!is.na(best.pars)]
		
		cm <- train(learn.task, subset=train.i, parset=best.pars2)                
		pred <- predict(learn.task, cm, newdata=learn.task@data[test.i,]) 
		cl <- learn.task["target.name"]
		test.perf <- performance(pred, learn.task@data[test.i,cl], learn.task@weights[test.i], measure)
		result[i, "test.perf"] <- test.perf
	}
	
	if (all.tune.results)
		r <- list(performances = result, tune.results = tune.result)
	else
		r <- result
	return(r)
}




