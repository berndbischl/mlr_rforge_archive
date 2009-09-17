#' @include resample.desc.r
roxygen()

#' Description class for bootstrapping.
#' @exportClass bs.desc
#' @title bs.desc
#' @seealso \code{\link{make.bs.desc}}
setClass("bs.desc", 
		contains = c("resample.desc")
)                                                     


#' Create description object for bootstrapping.
#' @param iters Number of iterations
#' @rdname bs.desc-class

setMethod(
		f = "initialize",
		signature = signature("bs.desc"),
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="bs.instance", name="bootstrap", iters=iters)
		}
)

#' Generates a description object for a bootstrap. Usually only needed in \code{\link{benchmark}} 
#' to describe the inner resampling.
#' 
#' @param size [\code{\link{integer}}] \cr 
#'        Size of the data set to resample.
#' @param iters [\code{\link{integer}}] \cr 
#'              Number of generated subsets / resampling iterations.
#' 
#' @return A \code{\linkS4class{bs.desc}} object.
#' @export 
#' @seealso \code{\linkS4class{bs.desc}}, \code{\link{benchmark}}
#' @title make.bs.desc
make.bs.desc = function(size, iters) {
	return(new("bs.desc", iters=iters))
}

