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

#' Generates a description object for subsampling. Usually only needed in \code{\link{benchmark}} 
#' to describe the inner resampling.
#' @param split [numeric] \cr Proportion of data used for training set. Default is 2/3.
#' @param iters [integer] \cr Number of generated subsets / resampling iterations.
#' @return A code{\linkS4Class{subsample.desc}} object.
#' @export 
#' @seealso code{\linkS4Class{subsample.desc}}, code{\link{benchmark}}
make.subsample.desc = function(size, split=2/3, iters) {
	return(new("subsample.desc", split=split, iters=iters))
}


