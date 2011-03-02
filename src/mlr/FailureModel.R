#' @include WrappedModel.R
roxygen()

setClass(
  "FailureModel",
  contains = c("WrappedModel")
)


#' @rdname to.string

setMethod(
  f = "to.string",
  signature = signature("FailureModel"),
  def = function(x) {
    s = callNextMethod(x)
    return(paste(s, "\nTraining failed: ", x@learner.model, sep=""))
  }
)


