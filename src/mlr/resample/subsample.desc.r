#' @include ResampleDesc.R
roxygen()


setClass("subsample.desc", 
		contains = c("ResampleDesc.nonseq"),
		representation = representation(split = "numeric")
)               


setMethod(
		f = "initialize",
		signature = signature("subsample.desc"),
		def = function(.Object, iters=30L, split=2/3,  ...) {
			.Object@split <- split
			callNextMethod(.Object, "subsample.instance", "subsampling", iters)
		}
)



