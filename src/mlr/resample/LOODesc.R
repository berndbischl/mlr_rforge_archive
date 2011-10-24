#' @include ResampleDesc.R
roxygen()

setClass("LOODesc", 
  contains = c("ResampleDesc")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("LOODesc"),
  def = function(.Object, iters=NA, ...) {
    callNextMethod(.Object, "loo.instance", "LOO", iters)
  }
)

