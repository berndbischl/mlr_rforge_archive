#' @include resample.desc.r
roxygen()


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





