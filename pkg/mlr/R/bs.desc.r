#' @include resample.desc.r
roxygen()

setClass("bs.desc", 
		contains = c("resample.desc.nonseq")
)                                                     


setMethod(
		f = "initialize",
		signature = signature("bs.desc"),
		def = function(.Object, iters, reps) {
			callNextMethod(.Object, "bs.instance", "bootstrap", iters, has.groups=FALSE)
		}
)


