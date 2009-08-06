#' @include resample.desc.r
roxygen()

#' Description class for subsampling
#' @exportClass subsample.desc

setClass("subsample.desc", 
		contains = c("resample.desc"),
		representation = representation(split = "numeric")
)               


#' Create description object for subsampling
#' @param Proportion of data used for training set
#' @param iters Number of iterations

setMethod(
		f = "initialize",
		signature = signature("subsample.desc"),
		def = function(.Object, split, iters) {
			.Object@split <- split
			callNextMethod(.Object, instance.class="subsample.instance", name="subsampling", iters=iters)
		}
)
