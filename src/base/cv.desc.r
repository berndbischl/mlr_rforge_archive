#' @include resample.desc.r
roxygen()

#' Description class for cross-validation
#' @exportClass cv.desc
#' @title cv.desc

setClass("cv.desc", 
		contains = c("resample.desc")
)                                                     


#' Create description object for cross-validation
#' @param iters Number of iterations

setMethod(
		f = "initialize",
		signature = signature("cv.desc"),
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="cv.instance", name="cross-validation", iters=iters)
		}
)


#' Generates a description object for a cross-validation. Usually only needed in \code{\link{benchmark}} 
#' to describe the inner resampling - e.g. for a double cross-validation.
#' @param iters [integer] \cr Number of generated subsets / resampling iterations.
#' @return A \code{\linkS4class{cv.desc}} object.
#' @export 
#' @seealso \code{\linkS4class{cv.desc}}, \code{\link{benchmark}}
make.cv.desc = function(size, iters) {
	return(new("cv.desc", iters=iters))
}



