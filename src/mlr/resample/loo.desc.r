#' @include ResampleDesc.R
roxygen()

setClass("loo.desc", 
  contains = c("ResampleDesc.nonseq")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("loo.desc"),
  def = function(.Object, iters=NA, ...) {
    callNextMethod(.Object, "loo.instance", "LOO", iters)
  }
)

