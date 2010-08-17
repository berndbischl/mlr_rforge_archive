
#' Generates an instance object for a resampling strategy. 
#' 
#' @param x [\code{\link{integer}}] \cr
#'        
#' @param task [\code{\link{integer}}] \cr
#'        Data of task to resample from.
#' @param size [\code{\link{integer}}] \cr
#'        Size of the data set to resample.
#' @param ... [any] \cr
#'		Further parameters for strategies. 
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
			make.res.i(cc, desc=desc, size=size)
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
			make.res.i(cc, desc=desc, size=task["size"], blocking=task["blocking"])
		}
)


#' @export 
#' @rdname make.res.instance

setMethod(
		f = "make.res.instance",
		signature = c(x="resample.desc", task="missing", size="numeric"),
		def = function(x, task, size, ...) {
			make.res.i(x@instance.class, desc=x, size=size)
		}
)

#' @export 
#' @rdname make.res.instance

setMethod(
		f = "make.res.instance",
		signature = c(x="resample.desc", task="learn.task", size="missing"),
		def = function(x, task, size, ...) {
			make.res.i(x@instance.class, desc=x, size=task["size"], blocking=task["blocking"])
		}
)


make.res.i = function(i.class, desc, size, blocking=factor(c())) {
	if (length(blocking) > 1) {
		levs = levels(blocking)
		size2 = length(levs)
		# create instance for blocks
		inst = new(i.class, desc=desc, size=size2)
		inds.list = i
		# now exchange block indices with shuffled indices of elements of this block
		f = function(i) sample(which(blocking == levs[i]))
		g = function(inds) Reduce(c, lapply(inds, f))
		inst@inds = lapply(inst@inds, g) 
		inst@size = size
	} else { 
		inst = new(i.class, desc=desc, size=size)
	}
	return(inst)
}
