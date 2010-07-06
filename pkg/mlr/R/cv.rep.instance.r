#' @include resample.instance.r
#' @include cv.rep.desc.r
roxygen()



setClass(
		"repcv.instance", 
		contains = c("resample.instance.nonseq")
)                                                     

setMethod(
		f = "initialize",
		signature = signature("repcv.instance"),
		def = function(.Object, desc, size) {
			n = desc["iters"]
			m = desc@props$reps
			inds = replicate(n, make.res.instance("cv", iters=m, size=size)@inds, simplify=F)
			inds = Reduce(c, inds)
			callNextMethod(.Object, desc=desc, size=size, inds=inds)
		}
)


