#' @include resample.desc.r
roxygen()

#' @export


setClass("bs.desc", 
		contains="resample.desc"
)                                                     

setMethod(
		f = "initialize",
		signature = "bs.desc",
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="bs.instance", name="bootstrap", iters=iters)
		}
)

setMethod(
		f = "[",
		signature = "bs.desc",
		def = function(x,i,j,...,drop) {
			return(callNextMethod())
		}
)