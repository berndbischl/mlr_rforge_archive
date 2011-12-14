#' @include ResampleDesc.R
roxygen()


setClass("SubsampleDesc", 
		contains = c("ResampleDesc"),
		representation = representation(split = "numeric")
)               


setMethod(
  f = "initialize",
  signature = signature("SubsampleDesc"),
  def = function(.Object, iters=30L, split=2/3,  ...) {
    .Object@split = split
    callNextMethod(.Object, "subsample.instance", "subsampling", iters)
  }
)

setMethod("show", "SubsampleDesc", function(object) {
  catf("%s with %i iterations and %.2f split rate.", object@id, object@iters, object@split)
  catf("Predict: %s", object@predict)
})


