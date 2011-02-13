#' @include ResampleDesc.r
roxygen()


setClass("cv.desc", 
		contains = c("ResampleDesc.nonseq")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("cv.desc"),
		def = function(.Object, iters, ...) {
			callNextMethod(.Object, "cv.instance", "cross-validation", iters)
		}
)





