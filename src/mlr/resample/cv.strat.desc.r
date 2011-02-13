#' @include ResampleDesc.r
roxygen()


setClass("stratcv.desc", 
  contains = c("ResampleDesc.nonseq")
)                                                     



setMethod(
  f = "initialize",
  signature = signature("stratcv.desc"),
  def = function(.Object, iters, ...) {
    callNextMethod(.Object, "stratcv.instance", "stratified cross-validation", iters)
  }
)





