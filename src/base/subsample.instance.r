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

#' @export
#' make.subsample.instance generates indices which represent training and test sets. 
#' @param size[numeric] \cr With size the training plus test set size is specified, normally it is the number of examples in the dataset.
#' @param split [numeric] \cr How many percent of the whole dataset is used as training set is defined by split. 
#' @param iters [numeric] \cr Iters is the number of generated subsets.
#' 
#' @return A list with the iters-number of training set indices is returned.
#' 
#' @example 
#' data(iris)
#' # split is the training set percentage
#' rin <- make.subsample.instance(iters=10, size=nrow(iris), split=2/3)
#' # holdout
#' rin <- make.subsample.instance(iters=1, size=nrow(iris), split=2/3)
#' 
#' @seealso \code{\linkS4class{resample.fit}}
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
