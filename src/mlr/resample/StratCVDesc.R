#' @include ResampleDesc.R
roxygen()


setClass("StratCVDesc", 
  contains = c("ResampleDesc.nonseq")
)                                                     



setMethod(
  f = "initialize",
  signature = signature("StratCVDesc"),
  def = function(.Object, iters, ...) {
    callNextMethod(.Object, "stratcv.instance", "stratified cross-validation", iters)
  }
)





