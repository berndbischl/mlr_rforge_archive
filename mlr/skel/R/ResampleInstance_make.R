#' Instantiates a resampling strategy object. 
#' 
#' @param desc [\code{\link{ResampleDesc}}] \cr
#'   Resampling description.
#' @param task [\code{\link{LearnTask}}] \cr
#'		Data of task to resample from. Prefer to pass this instead of \code{size}.
#' @param size [\code{\link{integer}}] \cr
#'		Size of the data set to resample. Can be used instead of \code{task}.
#' @return A \code{\link{ResampleInstance}} object.
#' @export 
#' @seealso \code{\link{makeResampleDesc}}, \code{\link{resample}} 
makeResampleInstance = function(desc, task, size) {
  checkArg(desc, "ResampleDesc")
  if (!missing(task)) {
    checkArg(task, "LearnTask")
    size = task@desc@size
    blocking = task@blocking
  } else{
    task = NULL
    blocking = factor(c())
  }
  if (!missing(size)) {
    size = convertInteger(size)
    checkArg(size, "integer", len=1, na.ok=FALSE)
  }
	make.res.i(desc@instance.class, desc, task, size, blocking)
}

make.res.i = function(i.class, desc, task, size, blocking=factor(c())) {
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
