#' @include resample.desc.r
roxygen()


setClass("repcv.desc", 
		contains = c("resample.desc.nonseq")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("repcv.desc"),
		def = function(.Object, iters, reps=10L) {
			callNextMethod(.Object, instance.class="repcv.instance", name="repeated cv", iters=iters, reps=reps)
		}
)



