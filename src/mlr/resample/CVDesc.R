#' @include ResampleDesc.R
roxygen()


setClass("CVDesc", 
		contains = c("ResampleDesc")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("CVDesc"),
		def = function(.Object, iters, ...) {
			callNextMethod(.Object, "CVInstance", "cross-validation", iters)
		}
)





