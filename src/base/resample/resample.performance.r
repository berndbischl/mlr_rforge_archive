#' @include resample.result.r
roxygen()


setGeneric(
		name = "resample.performance",
		def = function(task, result, measures, losses, aggr) {
			if (missing(measures))
				measures = default.measures(task)
			if (missing(losses))
				losses = list()
			if (missing(aggr)) {
				aggr=default.aggr(task)
			}
			measures = make.measures(measures)
			losses = lapply(losses, make.loss)
			standardGeneric("resample.performance")
		}
)

#' Measures the quality of predictions w.r.t. some loss function for a resampled fit.
#' 
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learn task.   	
#' @param result [\code{\linkS4class{resample.result}}] \cr
#'        Result from call to \code{\link{resample.fit}}.
#' @param loss [\code{\linkS4class{learn.task}}] \cr
#'        Learn task.   	
#' @param aggr1 [\code{\link{function}}] \cr
#'        Function used to aggregate performance values on test sets. Default is mean.   	
#' @param aggr2 [\code{\link{function}}] \cr
#'        Function used to aggregate indiviual losses of test cases to form a single test set performance value per test set. Default is mean.   	
#' @param spread [\code{\linkS4class{learn.task}}] \cr
#'        Function to calculate spread of performance values of test sets. Default is standard deviation (sd).   	
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
#' rt <- make.regr.task(data=BostonHousing, target="medv")
#' # do 3-fold crossvalidation
#' cv.i <- make.res.instance("cv", rt, iters=3) 
#' rf <- resample.fit("stats.lm", rt, cv.i)
#' # mean squared error 
#' resample.performance(rt, rf)
#' # median of absolute errors on test sets, aggregated by mean
#' resample.performance(rt, rf, loss="abs")		
#' 
#' @title Performance of a resample.fit

setMethod(
		f = "resample.performance",
		signature = c(task="learn.task", result="resample.result", measures="list", losses="list", aggr="list"),
		def = function(task, result, measures, losses, aggr) {
			n <- result["iters"]
			rin <- result["instance"]
			is = 1:n
			perfs = lapply(result@preds, function(p) performance(p, task=task, measures=measures, losses=losses))
			ms = Reduce(rbind, lapply(perfs, function(x) x$measure))
			ms2 = lapply(aggr, function(f) apply(ms, 2, f))
			ms2 = Reduce(rbind, ms2)
			ms = as.data.frame(rbind(ms2, ms))
			colnames(ms) = names(measures)
			rownames(ms) = c(names(aggr), is)

			ls = lapply(perfs, function (x) x$losses)
			ls = as.data.frame(Reduce(rbind, ls))
			
			if (nrow(ls) > 0)
				return(list(measures=ms, losses=ls))
			return(
				list(measures=ms))
		}
)

