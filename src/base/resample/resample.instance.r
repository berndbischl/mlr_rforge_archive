#' @include resample.desc.r
roxygen()

#' Base class for specific resampling draws like cross-validation or bootstrapping.
#' This class encapsulates training and test sets generated from the data set for a number of iterations. 
#' It mainly stores a set of integer vectors indicating the training and test examples for each iteration.
#' Don't create objects from this class directly but use the corresponding subclasses.
#' For construction simply use the factory method \code{\link{make.res.instance}}. 
#' 
#' Getter.
#' 
#' \describe{
#'  \item{desc [\code{\linkS4class{resample.desc}}]}{Resample description.}
#' 	\item{size [integer]}{Number of observations.}
#'  \item{train.inds [list]}{List of of training indices for all iterations.}
#'  \item{test.inds [list]}{List of test indices for all iterations.}
#'  \item{predict [factor]}{What to predict during resampling: "train", "test" or "both" sets.}
#'  \item{group [factor]}{Optional grouping of resampling iterations.}
#' }
#' 
#' @rdname resample.instance-class
#' 
#' @note If you want to add another resampling strategy, have a look at the web documentation. 
#' @exportClass resample.instance
#' @seealso \code{\linkS4class{resample.desc}}, \code{\link{make.res.instance}}, \code{\link{resample}} 
#' @title Resampling instance.


# todo validation for size
setClass(
		"resample.instance",   
		contains = c("object"), 
		# we always have to store training inds because the order might matter
		representation = representation(
				desc = "resample.desc", 
				size = "integer",
				train.inds = "list",
        test.inds = "list",
        predict = "factor",
        group = "factor"
    )
)


#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("resample.instance"),
		def = function(.Object, desc, size, train.inds, test.inds, group=c()) {
			if (missing(desc))
				return(.Object)
			.Object@desc = desc
			if (round(size) != size)
				error("You passed a non-integer to arg 'size' of resample.instance!")
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
      .Object@predict = factor(rep("test", desc["iters"]), levels=c("train", "test", "both"))
      .Object@group = group
      return(.Object)
		}
)

#' @rdname resample.instance-class

setMethod(
		f = "[",
		signature = signature("resample.instance"),
		def = function(x,i,j,...,drop) {
      if (i == "iters")
        return(length(x@train.inds))
      callNextMethod(x,i,j,...,drop) 
		}
)



#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("resample.instance"),
		def = function(x) {
			return(
					paste(
							"Instance for ", x@desc@id,  " with ", x["iters"], " iterations and ", x@size, " cases",
							sep=""
					)
			)
		}
)


setClass(
		"resample.instance.seq", 
		contains = c("resample.instance")
)


setClass(
		"resample.instance.nonseq", 
		contains = c("resample.instance")
)


