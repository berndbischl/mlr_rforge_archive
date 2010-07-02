#' @include resample.desc.r
roxygen()


setClass("subsample.desc", 
		contains = c("resample.desc.nonseq"),
		representation = representation(split = "numeric")
)               


setMethod(
		f = "initialize",
		signature = signature("subsample.desc"),
		def = function(.Object, split=2/3, iters=50L, group.iters=as.integer(NA)) {
			.Object@split <- split
			callNextMethod(.Object, "subsample.instance", "subsampling", iters, group.iters)
		}
)



