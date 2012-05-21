#' Instantiates a resampling strategy object. 
#' 
#' This class encapsulates training and test sets generated from the data set for a number of iterations. 
#' It mainly stores a set of integer vectors indicating the training and test examples for each iteration.
#'
#' Object slots:
#' \describe{
#' \item{desc [\code{\link{ResampleDesc}}]}{See argument.} 
#' \item{size [integer(1)]}{See argument.} 
#' \item{train.inds [list of \code{integer}]}{List of of training indices for all iterations.} 
#' \item{test.inds [list of \code{integer}]}{List of of test indices for all iterations.} 
#' \item{group [\code{factor}]}{Optional grouping of resampling iterations. This encodes whether specfic iterations 'belong together' (e.g. repeated CV), and it can later be used to aggregate performance values accordingly. Default is 'factor()'. }
#' }
#'
#' @param desc [\code{\link{ResampleDesc}}]\cr
#'   Resampling description object.
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   Data of task to resample from. Prefer to pass this instead of \code{size}.
#' @param size [\code{\link{integer}}]\cr
#'   Size of the data set to resample. Can be used instead of \code{task}.
#' @return [\code{\link{ResampleInstance}}].
#' @export 
#' @seealso \code{\link{makeResampleDesc}}, \code{\link{resample}} 
makeResampleInstance = function(desc, task, size) {
  checkArg(desc, "ResampleDesc")
  if (!missing(task)) {
    checkArg(task, "SupervisedTask")
    size = task$task.desc$size
    blocking = task$blocking
  } else{
    task = NULL
    blocking = factor(c())
  }
  if (!missing(size)) {
    size = convertInteger(size)
    checkArg(size, "integer", len=1L, na.ok=FALSE)
  }
  
  instantiate = function(size) {
    method = paste("makeResampleInstance", desc, sep="")
    args = c(iters=iters, list(...))
    d = do.call(method, args)
  }

  if (length(blocking) > 1) {
    if (is(desc, "StratCVDesc"))
      stop("Blocking can currently not be mixed with stratification in resampling!")
    if (is(desc, "RepCVDesc"))
      stop("Blocking can currently not be mixed with RepCV!")
    levs = levels(blocking)
		size2 = length(levs)
		# create instance for blocks
		inst = makeResampleInstanceInternal2(i.class, desc=desc, size=size2)
		# now exchange block indices with indices of elements of this block and shuffle
    inst$train.inds = lapply(inst$train.inds, function(i) sample(which(blocking %in% levs[i]))) 
    ti = sample(1:size)
    inst$test.inds = lapply(inst$train.inds, function(x)  setdiff(ti, x))
    inst$size = size
	} else { 
		inst = instantiate(, desc=desc, size=size, task=task)
	}
}

makeResampleInstanceInternal = function(desc, size, train.inds, test.inds, group) {
  if (missing(test.inds) && !missing(train.inds)) {
    # shuffle data set and remove inds
    test.inds = sample(1:size)
    test.inds = lapply(train.inds, function(x)  setdiff(test.inds, x))
  }
  if (!missing(test.inds) && missing(train.inds)) {
    # shuffle data set and remove inds
    train.inds = sample(1:size)
    train.inds = lapply(test.inds, function(x) setdiff(train.inds, x))
  }
  if (length(train.inds) != length(test.inds))
    error("Lengths of 'train.inds' and 'test.inds' must be equal!")
  inst = structure(list(
    desc=desc, 
    size=size,
    train.inds = train.inds,
    test.inds = test.inds,
    group = group
  ), class="ResampleInstance")
}

print.ResampleInstance = function(x, ...) { 
  catf("Resample instance on %i cases for:", object$size)
  print(object$desc) 
}
