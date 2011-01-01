#' @include object.r
roxygen()



setClass(
  "aggr",
  contains = c("object"),
  representation = representation(
    id = "character",
    fun = "function"
  )
)

#' @rdname measure-class

setMethod(
  f = "[",
  signature = signature("aggr"),
  def = function(x,i,j,...,drop) {
    callNextMethod()
  }
)


test.mean = new("aggr",
  id = "test.mean",
  fun = function(perf.test, perf.train, group, pred) mean(perf.test)
)

test.sd = new("aggr",
  id = "test.sd",
  fun = function(perf.test, perf.train, group, pred) sd(perf.test)
)

test.median = new("aggr",
  id = "test.median",
  fun = function(perf.test, perf.train, group, pred) median(perf.test)
)

test.min = new("aggr",
  id = "test.min",
  fun = function(perf.test, perf.train, group, pred) min(perf.test)
)

test.max = new("aggr",
  id = "test.max",
  fun = function(perf.test, perf.train, group, pred) max(perf.test)
)

#' Set how this measure will be aggregated after resampling. 
#' 
#' Possible are: test.mean, test.sd, test.median, test.min, test.max
#' They all calculate the respective statistic of the test set performances. 
#' 
#' @param measure [\code{\linkS4class{measure}}]\cr 
#'        Performance measure.   
#' @param aggr [\code{\linkS4class{aggr}}] \cr
#'        Aggregation function.
#'        
#' @return \code{\linkS4class{measure}} with changed aggregation behaviour.
#' @exportMethod set.aggr
#' @title Set aggregation functions of measure. 
#' @rdname set.aggr.pars 

set.aggr = function(measure, aggr) {
  if (!is.list(aggr))
    aggr = list(aggr)
  x@aggr = aggr
  return(x)
}

