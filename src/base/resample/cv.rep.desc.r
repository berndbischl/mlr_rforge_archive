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
		def = function(.Object, iters, reps=10L, ...) {
      if (!is.numeric(reps) || length(reps) != 1)
        stop("Argument 'reps' must be numeric and of length 1!")
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
        x["id"],  " with ", x@iters, " iterations and ", x["reps"] ," repetitions.\n",
        sep=""
      )
    )
  }
)


