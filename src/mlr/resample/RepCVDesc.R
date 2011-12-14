#' @include ResampleDesc.R
roxygen()


setClass("RepCVDesc", 
  contains = c("ResampleDesc"),
  representation = representation(
    reps = "integer"
  )
)                                                     



setMethod(
		f = "initialize",
		signature = signature("RepCVDesc"),
		def = function(.Object, iters, reps, folds, ...) {
      if (!is.numeric(reps) || length(reps) != 1)
        stop("Argument 'reps' must be integer and of length 1!")
      if (!is.numeric(folds) || length(folds) != 1)
        stop("Argument 'folds' must be integer and of length 1!")
      if (iters != reps * folds)
        stop("Argument 'iters' must be 'reps' x 'folds'")
      .Object@reps=as.integer(reps)
      callNextMethod(.Object, instance.class="RepCVInstance", id="repeated cv", iters=iters)  
		}
)

setMethod("show", "RepCVDesc", function(object) {
  catf("%s with %i iterations: %i folds and %i reps.", object@id, object@iters, object@iters/object@reps, object@reps)
  catf("Predict: %s", object@predict)
})

