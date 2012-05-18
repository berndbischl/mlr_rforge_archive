#' Resampling instance.
#' 
#' Base class for specific resampling draws like cross-validation or bootstrapping.
#' This class encapsulates training and test sets generated from the data set for a number of iterations. 
#' It mainly stores a set of integer vectors indicating the training and test examples for each iteration.
#' Don't create objects from this class directly but use the corresponding subclasses.
#' For construction simply use the factory method \code{\link{makeResampleInstance}}. 
#' 
#' @slot desc [\code{\link{ResampleDesc}}] Resample description that was used to create the instance. 
#' @slot size [integer(1)] Number of observations.  
#' @slot train.inds [list of integer] List of of training indices for all iterations. 
#' @slot test.inds [list of integer] List of of test indices for all iterations. 
#' @slot group [factor] Optional grouping of resampling iterations. This encodes whether specfic iterations 'belong together' (e.g. repeated CV), and it can later be used to aggregate performance values accordingly. Default is 'factor()'. 
#' 
#' @rdname ResampleInstance-class
#' 
#' @note If you want to add another resampling strategy, have a look at the web documentation. 
#' @exportClass ResampleInstance
#' @seealso \code{\link{ResampleDesc}}, \code{\link{makeResampleInstance}}, \code{\link{resample}} 



#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("ResampleInstance"),
		def = function(.Object, desc, size, train.inds, test.inds, group=factor()) {
			if (missing(desc))
				return(.Object)
			.Object@desc = desc
			if (round(size) != size)
				error("You passed a non-integer to arg 'size' of ResampleInstance!")
			.Object@size = as.integer(size)
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
      .Object@train.inds = train.inds
      .Object@test.inds = test.inds
      .Object@group = group
      return(.Object)
		}
)

print.RepCVDesc = function(x, ...) { 
  setMethod("show", "ResampleInstance", function(object) {
      catf("Resample instance on %i cases for:", object@size)
      print(object@desc) 
    })
  
setMethod(
  f = "initialize",
  signature = signature("LOODesc"),
  def = function(.Object, iters, ...) {
  }
)


