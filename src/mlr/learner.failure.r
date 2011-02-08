#' @include wrapped.model.r
roxygen()

setClass(
  "failure.model",
  contains = c("wrapped.model")
)

#' Getter.
#' @rdname failure.model-class

setMethod(
  f = "[",
  signature = signature("failure.model"),
  def = function(x,i,j,...,drop) {
    if (i == "fail"){
      return(x@learner.model)
    }
    callNextMethod()
  }
)



#' @rdname to.string

setMethod(
  f = "to.string",
  signature = signature("failure.model"),
  def = function(x) {
    s = callNextMethod(x)
    return(paste(s, "\nTraining failed: ", x["fail"], sep=""))
  }
)


