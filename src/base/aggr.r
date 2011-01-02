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


#' @export test.mean
test.mean = new("aggr",
  id = "test.mean",
  fun = function(perf.test, perf.train, group, pred) mean(perf.test)
)

#' @export test.sd
test.sd = new("aggr",
  id = "test.sd",
  fun = function(perf.test, perf.train, group, pred) sd(perf.test)
)

#' @export test.median
test.median = new("aggr",
  id = "test.median",
  fun = function(perf.test, perf.train, group, pred) median(perf.test)
)

#' @export test.min
test.min = new("aggr",
  id = "test.min",
  fun = function(perf.test, perf.train, group, pred) min(perf.test)
)

#' @export test.max
test.max = new("aggr",
  id = "test.max",
  fun = function(perf.test, perf.train, group, pred) max(perf.test)
)


