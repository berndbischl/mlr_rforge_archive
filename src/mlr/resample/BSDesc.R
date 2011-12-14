#' @include ResampleDesc.R
roxygen()

setClass("BSDesc", 
		contains = c("ResampleDesc")
)                                                     


setMethod(
		f = "initialize",
		signature = signature("BSDesc"),
		def = function(.Object, iters, ...) {
			callNextMethod(.Object, "BSInstance", "bootstrap", iters)
		}
)


