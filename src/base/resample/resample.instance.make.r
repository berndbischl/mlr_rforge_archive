
#' Generates an instance object for a resampling strategy. 
#' 
#' @param method [\code{\link{integer}}] \cr
#'        
#' @param task [\code{\link{integer}}] \cr
#'        Data of task to resample from.
#' @param size [\code{\link{integer}}] \cr
#'        Size of the data set to resample.
#' @param ... Futher parameters for strategies.\cr 
#'        iters: Number of generated subsets / resampling iterations.
#'        split: Percentage of training cases for hold-out / subsampling .
#' 
#' @return A \code{\linkS4class{resample.instance}} object.
#' @export 
#' @rdname make.res.instance
#' @title Construct resampling instance

setGeneric(
		name = "make.res.instance",
		def = function(method, task, size, ...) {
			standardGeneric("make.res.instance")
		}
)

#' @export 


setMethod(
		f = "make.res.instance",
		signature = c(method="character", task="missing", size="numeric"),
		def = function(method, task, size, ...) {
			desc = make.res.desc(method, ...)
			cc = paste(method, "instance", sep=".")
			return(new(cc, desc=desc, size=size))
		}
)

#' @export 

setMethod(
		f = "make.res.instance",
		signature = c(method="character", task="learn.task", size="missing"),
		def = function(method, task, size, ...) {
			make.res.instance(method, size=task["size"], ...)
		}
)

