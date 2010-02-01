#' @include resample.desc.r
roxygen()

#' Description class for bootstrapping.
#' @exportClass bs.desc
#' @title bs.desc
#' @seealso \code{\link{make.res.desc}}
setClass("bs.desc", 
		contains = c("resample.desc")
)                                                     


#' Create description object for bootstrapping.
#' @param iters Number of iterations

setMethod(
		f = "initialize",
		signature = signature("bs.desc"),
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="bs.instance", name="bootstrap", iters=iters)
		}
)


