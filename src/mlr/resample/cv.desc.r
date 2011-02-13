#' @include ResampleDesc.R
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





