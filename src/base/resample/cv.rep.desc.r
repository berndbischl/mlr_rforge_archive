#' @include resample.desc.r
roxygen()


setClass("repcv.desc", 
		contains = c("resample.desc.nonseq")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("repcv.desc"),
		def = function(.Object, iters, group.iters) {
			if (missing(group.iters) || is.na(group.iters))
				group.iters = 10L
			callNextMethod(.Object, instance.class="repcv.instance", name="repeated cv", iters=iters, group.iters=group.iters)
		}
)



