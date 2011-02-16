#' @include ResampleInstance.r
#' @include subsample.desc.r
roxygen()


setClass(
		"subsample.instance", 
		contains = c("ResampleInstance.nonseq")
)                                                     



setMethod(
		f = "initialize",
		signature = signature("subsample.instance"),
		def = function(.Object, desc, size, task) {
			if (missing(desc))
				return(.Object)
			inds = lapply(1:desc["iters"], function(x) sample(1:size, size*desc["split"]))
			callNextMethod(.Object, desc=desc, size=size, train.inds=inds)
		}
)

