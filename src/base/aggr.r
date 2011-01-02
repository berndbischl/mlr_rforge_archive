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


