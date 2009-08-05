#' @include resample.desc.r
roxygen()

#' @export


setClass("subsample.desc", 
		contains = c("resample.desc"),
		representation(split = "numeric")
)                                                     

setMethod(
		f = "initialize",
		signature = signature("subsample.desc"),
		def = function(.Object, split, iters) {
			.Object@split <- split
			callNextMethod(.Object, instance.class="subsample.instance", name="subsampling", iters=iters)
		}
)
