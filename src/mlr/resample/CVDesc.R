#' @include ResampleDesc.R
roxygen()


setClass("CVDesc", 
		contains = c("ResampleDesc.nonseq")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("CVDesc"),
		def = function(.Object, iters, ...) {
			callNextMethod(.Object, "cv.instance", "cross-validation", iters)
		}
)





