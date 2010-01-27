#' @include resample.desc.r
roxygen()

#' Description class for cross-validation.
#' @exportClass cv.desc
#' @title cv.desc
#' @seealso \code{\link{make.cv.desc}}

setClass("cv.desc", 
		contains = c("resample.desc")
)                                                     


#' Create description object for cross-validation.
#' @param iters Number of iterations
setMethod(
		f = "initialize",
		signature = signature("cv.desc"),
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="cv.instance", name="cross-validation", iters=iters)
		}
)





