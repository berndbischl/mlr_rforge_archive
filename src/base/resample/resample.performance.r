#' @include resample.result.r
roxygen()



#' Measures the quality of predictions w.r.t. some loss function for a resampled fit.
#' 
#' @param result [\code{\linkS4class{resample.result}}] \cr
#'        Result from call to \code{\link{resample.fit}}.
#' @param aggr1 [\code{\link{function}}] \cr
#'        Function used to aggregate performance values on test sets. Default is mean.   	
#' @param aggr2 [\code{\link{function}}] \cr
#'        Function used to aggregate indiviual losses of test cases to form a single test set performance value per test set. Default is mean.   	
#' 
#' @return A list with the following entries:
#' 		\item{\code{values}}{Numeric vector of estimated performances for the resampling iterations.}
#' 		\item{\code{aggr}}{Aggregated performance of above performance values. Aggregation is defined by the performance measure, e.g. mean or median.}
#' 		\item{\code{spread}}{Spread of above performance values. Defined by the performance measure, e.g. standard deviation.}
#' 
#' @export
#' @rdname resample.performance
#' 
#' @usage resample.performance( result, loss, aggr1, aggr2, spread)
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
		f = "performance",
		signature = c(x="resample.prediction", measures="list", losses="list", aggr="list"),
		def = function(x, measures, aggr, losses, task) {
			n <- x["iters"]
			rin <- x["instance"]
			is = 1:n
			# todo: better use split here?
			ps = as.list(x)
			perfs = lapply(ps, function(p) performance(p, measures=measures, losses=losses, task=task))
			ms = Reduce(rbind, lapply(perfs, function(x) x$measure))
			# ensure a matrix if we just get a single row in ms
			if (!is.matrix(ms))
				ms = as.matrix(t(ms))
			ms2 = lapply(aggr, function(f) apply(ms, 2, f))
			j = which(names(aggr) == "combine")
			if (length(j) > 0) {
				ms2[[j]] = callNextMethod(x=x, measures=measures, losses=list(), aggr=list(), task=task)$measures
			}
			ms2 = Reduce(rbind, ms2)
			ms = as.data.frame(rbind(ms, ms2))
			colnames(ms) = names(measures)
			rownames(ms) = c(is, names(aggr))
			ls = lapply(is, function (i) {
				cbind(iter=i, perfs[[i]]$losses)
			} )
			ls = as.data.frame(Reduce(rbind, ls))
			
			if (length(losses) > 0)
				return(list(measures=ms, losses=ls))
			return(
				list(measures=ms))
		}
)

