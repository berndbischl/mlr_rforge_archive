
#' Mainly for internal use. Construct a \code{\linkS4class{resample.instance}} from a \code{\linkS4class{resample.desc}}. 
#' Convenience method, so one doesn't have to call the specific constructors of classes inheriting from resample.desc.
#' 
#' @param desc [\code{\linkS4class{resample.desc}}] \cr Describes the resampling strategy.
#' @param size [integer] \cr Number of observations to resample from. 
#'              
#' @return Object of corresponding subclass of \code{\linkS4class{resample.instance}}.
#' @export
#' @rdname make.resample.instance
#'	
#' @usage make.resample.instance(desc, size) 
#'
#' @examples 
#'   cv.d <- new("cv.desc", iters = 10)
#'   rin <- make.resample.instance(desc = cv.d, size = nrow(iris))

setMethod(
		f = "make.resample.instance",
		signature = signature(desc = "resample.desc", size = "numeric"),
		def = function(desc, size) {
			return(new(desc@instance.class, desc, size = size))
		}
)


setGeneric(
		name = "make.res.instance",
		def = function(method, task, size, ...) {
			standardGeneric("make.res.instance")
		}
)



setMethod(
		f = "make.res.instance",
		signature = c(method="character", task="missing", size="numeric"),
		def = function(method, task, size, ...) {
			desc = make.res.desc(method, ...)
			cc = paste(method, "instance", sep=".")
			return(new(cc, desc=desc, size=size))
		}
)

setMethod(
		f = "make.res.instance",
		signature = c(method="character", task="learn.task", size="missing"),
		def = function(method, task, size, ...) {
			make.res.instance(method, size=task["size"], ...)
		}
)

