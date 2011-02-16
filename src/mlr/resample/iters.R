#' @include ResampleDesc.R
roxygen()
#' @include ResampleInstance.R
roxygen()
#' @include bench.result.r
roxygen()


#' Get number of iterations in resampling.
#' @param x [\code{\linkS4class{ResampleDesc}} | \code{\linkS4class{ResampleInstance}} | \code{\linkS4class{bench.result}}] 
#'   Object to retrieve iterations from.
#' @return Number of iterations. 
#' @exportMethod iters
#' @rdname iters

setGeneric(
  name = "iters",
  def = function(x) {
    standardGeneric("resample.update")
  }
)

#' @rdname iters
#' @return [integer(1)]
setMethod(f = "iters", signature = signature("ResampleDesc"), def = function(x) x@iters)

#' @rdname iters
#' @return [integer(1)]
setMethod(f = "iters", signature = signature("ResampleInstance"), def = function(x) x@desc@iters)

#' @rdname iters
#' @return [integer(number of tasks)] Iterations per task.
setMethod(f = "iters", signature = signature("bench.result"), def = function(x) sapply(x@resamplings, function(y) y@desc@iters))

