#' @include resample.desc.r
roxygen()

#' @export


setClass("cv.desc", 
		contains = c("resample.desc")
)                                                     

setMethod(
		f = "initialize",
		signature = signature("cv.desc"),
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="cv.instance", name="cross-validation", iters=iters)
		}
)

setMethod(
		f = "[",
		signature = signature("cv.desc"),
		def = function(x,i,j,...,drop) {
			if (i == "folds")
				return(callNextMethod(x,"iters",j,drop=drop))
			else 
				return(callNextMethod())
		}
)