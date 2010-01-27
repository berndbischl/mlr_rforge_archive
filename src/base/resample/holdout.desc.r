#' @include resample.desc.r
roxygen()


#' Description class for hold-out.
#' @exportClass holdout.desc
#' @seealso \code{\link{make.res.desc}}
setClass("holdout.desc", 
		contains = c("subsample.desc")
)               


#' Create description object for hold-out.
#' @param Proportion of data used for training set

setMethod(
		f = "initialize",
		signature = signature("holdout.desc"),
		def = function(.Object, split=2/3) {
			callNextMethod(.Object, split=split, iters=1)
		}
)

