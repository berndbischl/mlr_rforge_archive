#' @include resample.instance.r
#' @include subsample.desc.r
roxygen()

#' @export


setClass("subsample.instance", contains="resample.instance")                                                     

setMethod(
		f = "initialize",
		signature = "subsample.instance",
		def = function(.Object, desc, size) {
			inds <- lapply(1:desc["iters"], function(x) sample(1:size, size*desc["split"]))
			callNextMethod(.Object, desc=desc, size=size, inds=inds)
		}
)


setGeneric(
		name = "make.subsample.instance",
		def = function(size, split, iters) {
			if (missing(split))
				split = 2/3
			if (missing(iters))
				iters = 1
			standardGeneric("make.subsample.instance")
		}
)

#' @export
setMethod(
		f = "make.subsample.instance",
		signature = c(size="numeric", split="numeric", iters="numeric"),
		def = function(size, split, iters) {
			desc <- new("subsample.desc", iters=iters, split=split)
			return(new("subsample.instance", desc=desc, size=size))
		}
)
