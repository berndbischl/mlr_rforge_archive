
#' Generates an instance object for a resampling strategy. 
#' 
#' @param x [\code{\link{integer}}] \cr
#'        
#' @param task [\code{\link{integer}}] \cr
#'        Data of task to resample from.
#' @param size [\code{\link{integer}}] \cr
#'        Size of the data set to resample.
#' @param ... [any] \cr
#'		Futher parameters for strategies. 
#'		iters: Number of generated subsets / resampling iterations.
#'		split: Percentage of training cases for hold-out / subsampling .
#' 
#' @return A \code{\linkS4class{resample.instance}} object.
#' @export 
#' @rdname make.res.instance
#' @title Construct resampling instance

setGeneric(
		name = "make.res.instance",
		def = function(x, task, size, ...) {
			standardGeneric("make.res.instance")
		}
)

#' @export 
#' @rdname make.res.instance


setMethod(
		f = "make.res.instance",
		signature = c(x="character", task="missing", size="numeric"),
		def = function(x, task, size, ...) {
			desc = make.res.desc(x, ...)
			cc = paste(x, "instance", sep=".")
			return(new(cc, desc=desc, size=size))
		}
)

#' @export 
#' @rdname make.res.instance

setMethod(
		f = "make.res.instance",
		signature = c(x="character", task="learn.task", size="missing"),
		def = function(x, task, size, ...) {
			desc = make.res.desc(x, ...)
			cc = paste(x, "instance", sep=".")
			new(cc, desc=desc, size=task["size"])
		}
)


#' @export 
#' @rdname make.res.instance

setMethod(
		f = "make.res.instance",
		signature = c(x="resample.desc", task="missing", size="numeric"),
		def = function(x, task, size, ...) {
			new(x@instance.class, desc=x, size=size)
		}
)

#' @export 
#' @rdname make.res.instance

setMethod(
		f = "make.res.instance",
		signature = c(x="resample.desc", task="learn.task", size="missing"),
		def = function(x, task, size, ...) {
			new(x@instance.class, desc=x, size=task["size"])
		}
)

