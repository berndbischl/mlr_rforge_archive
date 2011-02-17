#' @include SubsampleDesc.R
roxygen()


setClass("HoldoutDesc", 
		contains = c("SubsampleDesc")
)               



setMethod(
		f = "initialize",
		signature = signature("HoldoutDesc"),
		def = function(.Object, iters, split=2/3, ...) {
			callNextMethod(.Object, split=split, iters=1L)
		}
)

