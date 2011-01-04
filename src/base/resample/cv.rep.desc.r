#' @include resample.desc.r
roxygen()


setClass("repcv.desc", 
  contains = c("resample.desc.nonseq"),
  representation = representation(
    reps = "integer"
  )
)                                                     



setMethod(
		f = "initialize",
		signature = signature("repcv.desc"),
		def = function(.Object, iters, reps=10L, folds=10L, ...) {
      if (!is.numeric(reps) || length(reps) != 1)
        stop("Argument 'reps' must be integer and of length 1!")
      if (!is.numeric(folds) || length(folds) != 1)
        stop("Argument 'folds' must be integer and of length 1!")
      if (iters==1)
        iters = as.integer(reps*folds)
      if (reps==10)
        reps = iters / folds
      if (folds==10)
        folds = iters / reps
      if (iters != reps * folds)
        stop("Argument 'iters' must be 'reps' x 'folds'")
      .Object@reps=as.integer(reps)
      callNextMethod(.Object, instance.class="repcv.instance", id="repeated cv", iters=iters)  
		}
)

#' @rdname to.string

setMethod(
  f = "to.string",
  signature = signature("repcv.desc"),
  def = function(x) {
    return(
      paste(
        x["id"],  " with ", x["iters"], " iterations: ", x["iters"]/x["reps"] ," folds and ", x["reps"] ," reps.\n",
        sep=""
      )
    )
  }
)


