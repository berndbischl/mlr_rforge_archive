#' @include resample.desc.r
roxygen()

#' Description class for subsampling.
#' @exportClass subsample.desc
#' @title subsample.desc
#' @seealso \code{\link{make.subsample.desc}}
setClass("subsample.desc", 
		contains = c("resample.desc"),
		representation = representation(split = "numeric")
)               


#' Create description object for subsampling.
#' @param Proportion of data used for training set
#' @param iters Number of iterations
#' @rdname subsample.desc-class

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
#' 
#' @param size [\code{\link{integer}}] \cr
#'        Size of the data set to resample.
#' @param split [\code{\link{numeric}}] \cr 
#'        Proportion of data used for training set. Default is 2/3.
#' @param iters [\code{\link{integer}}] \cr 
#'        Number of generated subsets / resampling iterations.
#' 
#' @return A \code{\linkS4class{subsample.desc}} object.
#' @export 
#' @seealso \code{\linkS4class{subsample.desc}}, \code{\link{benchmark}}
#' @title make.subsample.desc
make.subsample.desc = function(size, split=2/3, iters) {
	return(new("subsample.desc", split=split, iters=iters))
}


