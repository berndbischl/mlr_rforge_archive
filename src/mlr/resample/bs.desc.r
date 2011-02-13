#' @include ResampleDesc.r
roxygen()

setClass("bs.desc", 
		contains = c("ResampleDesc.nonseq")
)                                                     


setMethod(
		f = "initialize",
		signature = signature("bs.desc"),
		def = function(.Object, iters, ...) {
			callNextMethod(.Object, "bs.instance", "bootstrap", iters)
		}
)


