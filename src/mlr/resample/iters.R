#' @include ResampleDesc.R
roxygen()
#' @include ResampleInstance.R
roxygen()


#' Get number of iterations in resampling.
#' @param x [\code{\linkS4class{ResampleDesc}} | \code{\linkS4class{ResampleInstance}}] 
#'   Object to retrieve iterations from.
#' @return Number of iterations. 
#' @exportMethod iters
#' @rdname iters

setGeneric(
  name = "iters",
  def = function(x) {
    standardGeneric("iters")
  }
)

#' @rdname iters
#' @return [integer(1)]
setMethod(f = "iters", signature = signature("ResampleDesc"), def = function(x) x@iters)

#' @rdname iters
#' @return [integer(1)]
setMethod(f = "iters", signature = signature("ResampleInstance"), def = function(x) x@desc@iters)


