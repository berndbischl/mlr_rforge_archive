
#' Generates an instance object for a resampling strategy. 
#' 
#' @param desc [\code{\linkS4class{ResampleDesc}}] \cr
#'   Resampling description.
#' @param task [\code{\linkS4class{LearnTask}}] \cr
#'		Data of task to resample from. Prefer to pass this instead of \code{size}.
#' @param size [\code{\link{integer}}] \cr
#'		Size of the data set to resample. Can be used instead of \code{task}.
#' 
#' @return A \code{\linkS4class{ResampleInstance}} object.
#' @export 
#' @seealso code{\link{makeResampleDesc}}, \code{\link{resample}} 
#' @rdname makeResampleInstance
#' @title Construct resampling instance

setGeneric(
		name = "makeResampleInstance",
		def = function(desc, task, size) {
      if (!missing(size) && is.numeric(size))
        size = as.integer(size)
      standardGeneric("makeResampleInstance")
		}
)


#' @export 
#' @rdname makeResampleInstance

setMethod(
		f = "makeResampleInstance",
		signature = c(desc="ResampleDesc", task="missing", size="integer"),
		def = function(desc, task, size) {
			make.res.i(desc@instance.class, desc=desc, size=size, task=NULL)
		}
)

#' @export 
#' @rdname makeResampleInstance

setMethod(
		f = "makeResampleInstance",
		signature = c(desc="ResampleDesc", task="LearnTask", size="missing"),
		def = function(desc, task, size) {
			make.res.i(desc@instance.class, desc=desc, task=task, blocking=task["blocking"])
		}
)


make.res.i = function(i.class, desc, task=NULL, size=as.integer(NA), blocking=factor(c())) {
  if (!is.null(task)) {
    size = task["size"]
  }
	if (length(blocking) > 1) {
    if (is(desc, "StratCVDesc"))
      stop("Blocking can currently not be mixed with stratification in resampling!")
    if (is(desc, "RepCVDesc"))
      stop("Blocking can currently not be mixed with RepCV!")
    levs = levels(blocking)
		size2 = length(levs)
		# create instance for blocks
		inst = new(i.class, desc=desc, size=size2)
		# now exchange block indices with indices of elements of this block and shuffle
    inst@train.inds = lapply(inst@train.inds, function(i) sample(which(blocking %in% levs[i]))) 
    ti = sample(1:size)
    inst@test.inds = lapply(inst@train.inds, function(x)  setdiff(ti, x))
    inst@size = size
	} else { 
		inst = new(i.class, desc=desc, size=size, task=task)
	}
	return(inst)
}
