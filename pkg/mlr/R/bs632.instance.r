#' @include resample.instance.r
roxygen()
#' @include bs632.desc.r
roxygen()
#' @include get.train.test.r
roxygen()



setClass(
		"bs632.instance", 
		contains = c("resample.instance.nonseq"))                                                     


setMethod(
		f = "initialize",
		signature = signature("bs632.instance"),
		def = function(.Object, desc, size) {
			inds = boot(1:size, R=desc["iters"], function(data,inds) inds)$t
			inds = as.list(as.data.frame(t(inds)))
			names(inds) = NULL
			callNextMethod(.Object, desc=desc, size=size, inds=inds)
		}
)

#' @rdname get.test.set
#' @export
setMethod(
		f = "get.test.set",
		signature = signature("bs632.instance", "integer"),
		def = function(x, i) {
			i1 = x@inds[[i]]
			i2 = setdiff(1:x["size"], x@inds[[i]])
			g = as.factor(rep(c("train", "test"), c(length(i1), length(i2))))
			list(inds=c(i1, i2), group=g)
		}
)


