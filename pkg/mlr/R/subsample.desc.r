#' @include resample.desc.r
roxygen()


setClass("subsample.desc", 
		contains = c("resample.desc"),
		representation = representation(split = "numeric")
)               


setMethod(
		f = "initialize",
		signature = signature("subsample.desc"),
		def = function(.Object, split=2/3, iters=30) {
			.Object@split <- split
			callNextMethod(.Object, instance.class="subsample.instance", name="subsampling", iters=iters)
		}
)



