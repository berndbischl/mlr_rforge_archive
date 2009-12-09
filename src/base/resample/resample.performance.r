#' @include resample.result.r
roxygen()


setGeneric(
		name = "resample.performance",
		def = function(task, result, loss, aggr1, aggr2, spread) {
			if (missing(loss))
				loss <- default.loss(task)
			if (is.character(loss))
				loss <- make.loss(loss)
			if (missing(aggr1))
				aggr1=mean
			if (missing(aggr2))
				aggr2=mean
			if (missing(spread))
				spread=sd
			standardGeneric("resample.performance")
		}
)

#' Measures the quality of predictions w.r.t. some loss function for a resampled fit.
#' 
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'   	Specifies the learning task for the problem.
#' @param result [\code{\linkS4class{resample.result}}] \cr
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
#' @usage resample.performance(task, result, loss, aggr1, aggr2, spread)
#'
#' @examples
#' library(mlbench)
#' data(BostonHousing) 
#' # define a regression task for the Boston Housing problem with a simple linear modell
#' rt <- make.regr.task("lm", data=iris, target="medv")
#' # do 3-fold crossvalidation
#' cv.i <- make.cv.instance(size=nrow(BostonHousing), iters=3) 
#' rf <- resample.fit(rt, cv.i)
#' # mean squared error 
#' resample.performance(rt, rf)
#' # median of absolute errors
#' resample.performance(rt, rf, measure="mae")		
#' 
#' @title performance

setMethod(
		f = "resample.performance",
		signature = c(task="learn.task", result="resample.result", loss="loss", aggr1="function", aggr2="function", spread="function"),
		def = function(task, result, loss, aggr1, aggr2, spread) {
			n <- result["iters"]
			rin <- result["instance"]
			vals <- list()
			aggrs <- numeric(n)
			for(i in 1:n)  {
				trues.i <- get.test.targets(task, rin, i)
				preds.i <- result["fitted", i]
				w.i <- task@weights[rin["test.inds", i]]
				p <- performance(preds.i, trues.i, w.i, loss, aggr2)
				vals[[i]] = p$vals
				aggrs[i] = p$aggr
			}
						
			perf.aggr = aggr1(aggrs)
			perf.spread = ifelse(is.na(perf.aggr), NA, spread(aggrs))
			res <- list(aggr1=perf.aggr, spread=perf.spread, aggr2=aggrs, vals=vals)
			#ag <- measure$aggr.name 
			#sp <- measure$spread.name
			#names(res)[[2]] <- ag
			#names(res)[[3]] <- sp
			return(res)
		}
)

