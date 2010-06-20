#' @include resample.desc.r
roxygen()

setClass("bs.desc", 
		contains = c("resample.desc")
)                                                     


setMethod(
		f = "initialize",
		signature = signature("bs.desc"),
		def = function(.Object, iters) {
			callNextMethod(.Object, instance.class="bs.instance", name="bootstrap", iters=iters)
		}
)


