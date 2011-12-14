#' @include SubsampleInstance.R
#' @include HoldoutDesc.R
roxygen()



setClass(
		"holdout.instance", 
		contains = c("subsample.instance")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("holdout.instance"),
		def = function(.Object, desc, size, task) {
			callNextMethod(.Object, desc=desc, size=size)
		}
)

