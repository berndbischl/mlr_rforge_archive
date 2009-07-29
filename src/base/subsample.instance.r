#' @include resample.instance.r
#' @include subsample.desc.r
roxygen()

#' @export


setClass("subsample.instance", contains="resample.instance")                                                     

setMethod(
		f = "initialize",
		signature = "subsample.instance",
		def = function(.Object, desc, size) {
			inds <- lapply(1:desc["iters"], function(x) sample(1:size, size*desc["split"]))
			callNextMethod(.Object, desc=desc, size=size, inds=inds)
		}
)


setGeneric(
		name = "make.subsample.instance",
		def = function(size, split, iters) {
			if (missing(split))
				split = 2/3
			if (missing(iters))
				iters = 1
			standardGeneric("make.subsample.instance")
		}
)

#' make.subsample.instance generates training and test set indices for subsampling. 
#' 
#' @param size [integer] \cr Size of the data set to resample.
#' @param split [numeric] \cr Percentage of data used for training set. 
#' @param iters [integer] \cr Number of generated subsets / resampling iterations.
#' 
#' @return A \code{\linkS4class{subsample.instance}} object, which encapsulates the generated indices of training and test sets.
#' 
#' @export
#' 
#' @examples 
#' data(iris)
#' # split is the training set percentage
#' rin <- make.subsample.instance(iters=10, size=nrow(iris), split=2/3)
#' # holdout
#' rin <- make.subsample.instance(iters=1, size=nrow(iris), split=2/3)
#' 
#' @seealso \code{\link{resample.fit}}
#' 
#' @title make.cv.instance


setMethod(
		f = "make.subsample.instance",
		signature = c(size="numeric", split="numeric", iters="numeric"),
		def = function(size, split, iters) {
			desc <- new("subsample.desc", iters=iters, split=split)
			return(new("subsample.instance", desc=desc, size=size))
		}
)
