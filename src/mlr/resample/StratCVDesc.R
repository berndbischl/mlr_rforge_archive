#' @include ResampleDesc.R
roxygen()


setClass("StratCVDesc", 
  contains = c("ResampleDesc")
)                                                     



setMethod(
  f = "initialize",
  signature = signature("StratCVDesc"),
  def = function(.Object, iters, ...) {
    callNextMethod(.Object, "StratCVInstance", "stratified cross-validation", iters)
  }
)





