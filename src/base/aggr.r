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


aggr.mean = new("aggr",
  id = "mean",
  fun = function(perf.test, perf.train, group, pred) mean(perf.test)
)


set.aggr = function(x, aggr) {
  if (!is.list(aggr))
    aggr = list(aggr)
  x@aggr = aggr
  return(x)
}

