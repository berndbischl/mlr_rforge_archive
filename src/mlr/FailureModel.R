#' @include WrappedModel.R
roxygen()

setClass(
  "FailureModel",
  contains = c("WrappedModel")
)


setMethod("show", "FailureModel", function(object) {
  s = callNextMethod(object)
  cat(s, "\nTraining failed: ", object@learner.model, "\n", sep="")
})