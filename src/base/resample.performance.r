#' @include resample.result.r
roxygen()



setGeneric(
		name = "resample.performance",
		def = function(learn.task, resample.instance, resample.result, measure) {
			if (missing(measure))
				measure <- make.default.measure(learn.task)
			if (is.character(measure))
				measure <- make.measure(measure)
			standardGeneric("resample.performance")
		}
)

#' Measures the quality of predictions w.r.t. some loss function for a resampled fit.
#' 
#' @param learn.task [\code{\linkS4class{learn.task}}] \cr
#'   	Specifies the learning task for the problem.
#' @param resample.instance [\code{\linkS4class{resample.instance}}] \cr
#'   	Specifies the training and test indices of the resampled data. 
#' @param resample.result [\code{\linkS4class{resample.result}}] \cr
#' @param measure [\code{\link{character}}/\code{\link{list}}] \cr 
#' 		Name of performance measure to optimize or a list describing your own performance measure. 
#' 		The default is mean misclassification error for classification or MSE for regression. 
#' 
#' @return A list with the following entries:
#' 		\item{\code{values}}{Numeric vector of estimated performances for the resampling iterations.}
#' 		\item{\code{aggr}}{Aggregated performance of above performance values. Aggregation is defined by the performance measure, e.g. mean or median.}
#' 		\item{\code{spread}}{Spread of above performance values. Defined by the performance measure, e.g. standard deviation.}
#' 
#' @export
#' @rdname resample.performance
#' 
#' @usage resample.performance(learn.task, resample.instance, resample.result, measure)
#'
#' @examples
#' library(mlbench)
#' data(BostonHousing) 
#' # define a regression task for the Boston Housing problem with a simple linear modell
#' rt <- make.regr.task("lm", data=iris, formula=medv~.)
#' # do 3-fold crossvalidation
#' cv.i <- make.cv.instance(size=nrow(BostonHousing), iters=3) 
#' rf <- resample.fit(rt, cv.i)
#' # mean squared error 
#' resample.performance(rt, cv.i, rf)
#' # median of absolute errors
#' resample.performance(rt, cv.i, rf, measure="mae")		
#' 
#' @title performance

setMethod(
		f = "resample.performance",
		signature = c(learn.task="learn.task", resample.instance="resample.instance", resample.result="resample.result", measure="list"),
		def = function(learn.task, resample.instance, resample.result, measure) {
			n <- resample.result["iters"]
			rin <- resample.instance
			perf <- numeric(n)
			for(i in 1:n)  {
				trues.i <- get.test.targets(learn.task, rin, i)
				preds.i <- resample.result["fitted", i]
				w.i <- learn.task@weights[rin["test.inds", i]]
				perf[i] <- performance(preds.i, trues.i, w.i, measure, costs=costs)
			}
			
			perf.aggr = measure$aggregate(perf)
			perf.spread = ifelse(is.na(perf.aggr), NA, measure$spread(perf))
			res <- list(values=perf, aggr=perf.aggr, spread=perf.spread)
			#ag <- measure$aggr.name 
			#sp <- measure$spread.name
			#names(res)[[2]] <- ag
			#names(res)[[3]] <- sp
			return(res)
		}
)

