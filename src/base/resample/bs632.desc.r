#' @include resample.desc.r
roxygen()

setClass("bs632.desc", 
		contains = c("resample.desc.nonseq")
)                                                     


setMethod(
		f = "initialize",
		signature = signature("bs632.desc"),
		def = function(.Object, iters, reps) {
			callNextMethod(.Object, "bs632.instance", "B632", iters)
		}
)


