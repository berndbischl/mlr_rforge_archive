#' @include resample.desc.r
roxygen()


setClass("subsample.desc", 
		contains = c("resample.desc.nonseq"),
		representation = representation(split = "numeric")
)               


setMethod(
		f = "initialize",
		signature = signature("subsample.desc"),
		def = function(.Object, split=2/3, iters=50L) {
			.Object@split <- split
			callNextMethod(.Object, "subsample.instance", "subsampling", iters, has.groups=FALSE)
		}
)



