#' @include ResampleDesc.R
roxygen()

setClass("LOODesc", 
  contains = c("ResampleDesc")
)                                                     

setMethod(
  f = "initialize",
  signature = signature("LOODesc"),
  def = function(.Object, iters, ...) {
    callNextMethod(.Object, "loo.instance", "LOO", iters=as.integer(NA))
  }
)

setMethod("show", "LOODesc", function(object) {
  catf("Leave-one-out.")
  catf("Predict: %s", object@predict)
})


