#' @include ResampleDesc.R
roxygen()


setClass("stratCVDesc", 
  contains = c("ResampleDesc.nonseq")
)                                                     



setMethod(
  f = "initialize",
  signature = signature("stratCVDesc"),
  def = function(.Object, iters, ...) {
    callNextMethod(.Object, "stratcv.instance", "stratified cross-validation", iters)
  }
)





