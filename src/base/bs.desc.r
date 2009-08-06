#' @include resample.desc.r
roxygen()

#' Description class for bootstrapping
#' @exportClass bs.desc
#' @title bs.desc
setClass("bs.desc", 
		contains = c("resample.desc")
)                                                     


#' Create description object for bootstrapping
#' @param iters Number of iterations

setMethod(
		f = "initialize",
		signature = signature("bs.desc"),
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="bs.instance", name="bootstrap", iters=iters)
		}
)

#' Generates a description object for a bootstrap. Usually only needed in \code{\link{benchmark}} 
#' to describe the inner resampling.
#' @param iters [integer] \cr Number of generated subsets / resampling iterations.
#' @return A \code{\linkS4class{bs.desc}} object.
#' @export 
#' @seealso \code{\linkS4class{bs.desc}}, \code{\link{benchmark}}
make.bs.desc = function(size, iters) {
	return(new("bs.desc", iters=iters))
}

