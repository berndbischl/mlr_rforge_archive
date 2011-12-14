#' @include SubsampleInstance.R
#' @include HoldoutDesc.R
roxygen()



setClass(
		"HoldoutInstance", 
		contains = c("SubsampleInstance")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("HoldoutInstance"),
		def = function(.Object, desc, size, task) {
			callNextMethod(.Object, desc=desc, size=size)
		}
)

