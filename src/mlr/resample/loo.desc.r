#' @include resample.desc.r
roxygen()

setClass("loo.desc", 
  contains = c("resample.desc.nonseq")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("loo.desc"),
  def = function(.Object, iters=NA, ...) {
    callNextMethod(.Object, "loo.instance", "LOO", iters)
  }
)

