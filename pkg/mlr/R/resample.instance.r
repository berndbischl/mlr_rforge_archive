#' @include resample.desc.r
roxygen()

#'  \describe{	
#' Base class for specific resampling draws like cross-validation and bootstrapping.
#' New training and test cases are generated from the data set for a number of iterations. 
#' It mainly stores a set of integer vectors indicating the training examples for each interation.
#' Don't create objects from this class directly but use the correpsonding subclasses.
#' For construction you can either first create a resample.desc (e.g. cv.desc) to describe 
#' this resampling strategy and then pass this to the correponding or (more convieniently)
#' invoke a direct construction method (e.g. make.cv.instance).  
#' }
#' 
#' \cr\cr\bold{Slots:}
#'  \describe{	
#'   \item{\code{size [numeric]}}{Number of observations in the data}
#'   \item{\code{inds [list]}}{List of integer vectors specifying the training cases for each iteration. Each vector might contain duplicated indices and the order matters for some classifiers.}
#'  }
#' 
#' \cr\cr\bold{Getter:}
#'  \describe{	
#'   \item{\code{data.size [single numeric]}}{Number of observations in the data}
#'   \item{\code{iters [single numeric]}}{Number of resampling interations.}
#'   \item{\code{train.inds (i) [(list of) integer vector(s) ]}}{If i is a single integer, the vector of trainings indices for the ith iteration is returned. If i is an integer vector, the list of training indices for the given iterations is returned.}
#'   \item{\code{test.inds (i) [(list of) integer vector(s) ]}}{If i is a single integer, the vector of test indices for the ith iteration is returned. If i is an integer vector, the list of test indices for the given iterations is returned.}
#'  }
#' 
#' \cr\cr\bold{Subclasses:}
#'  \describe{	
#'   \item{\link{cv.instance}}{Cross-validation}
#'   \item{\link{bs.instance}}{Bootstrapping}
#'   \item{\link{subsample.instance}}{Subsampling}
#'  }
#' 
#'  @title resample.run
#'  @note If you want to add another resampling strategy, simply inherit from resample.desc and this class and generate the training indices in the the constructor of the resample.instance according to your resampling strategy.
#'  @seealso \link{resample.desc}, \link{}, \link{resample.fit} 
#'  @export

# todo validation for size
setClass(
		"resample.instance",                                                     
		# we always have to store training inds because the order might matter
		representation(desc = "resample.desc", size="numeric", inds="list")
)

setMethod(
		f = "initialize",
		signature = "resample.instance",
		def = function(.Object, desc, size, inds) {
			if (missing(desc))
				return(.Object)
			.Object@desc <- desc
			.Object@size <- size
			.Object@inds <- inds
			return(.Object)
		}
)



setGeneric(
		name = "make.resample.instance",
		def = function(desc, size) {
			standardGeneric("make.resample.instance")
		}
)

#' Construct a resample run from a resample desc. Convenience method, so you don't have to call 
#' the specific constructors of classes inheriting from resample.desc.
#' 
#' @param desc [resample.desc] Object of a class inheriting from resample.desc. Describes the resampling strategy.
#' @param size [single integer] Size of the data set to resample from. 
#'              
#' @return Corresponding object inheriting from resample.instance.
#'
#' @usage (desc, size) 
#'
#' @examples 
#'   desc <- new("cv.desc", folds=10)
#'   cvr <- (desc=desc, size=size)
#'
#'  @export
setMethod(
		f = "make.resample.instance",
		signature = c(desc="resample.desc", size="numeric"),
		def = function(desc, size) {
			return(new(desc@instance.class, desc, size=size))
		}
)



#----------------- getter ---------------------------------------------------------


setMethod(
		f = "[",
		signature = "resample.instance",
		def = function(x,i,j,...,drop) {
			if (i == "data.size")
				return(x@size)
			
			if (i == "name")
				return(x@desc@name)
			
			if (i == "iters")
				return(length(x@inds))
			
			if (i == "train.inds") {
				if (missing(j)) {
					return(x@inds)
				} else if(length(j) == 1) {
					return(x@inds[[j]])
				}
				else {
					return(x@inds[j])
				}
			}
			
			if (i == "test.inds") {
				size <- x["data.size"]
				inds <- x@inds[j]
				if (length(j) == 1) {
					return( (1:size)[-inds[[1]]] )
				}
				else {
					return(lapply(inds, function(x) (1:size)[-x]))
				}
			}
			return(x@desc[i,j,...,drop])
		}
)

get.train.targets <- function(learn.task, resample.instance, i) {
	inds <- resample.instance["train.inds", i]
	return(learn.task["targets", inds])
}

get.test.targets <- function(learn.task, resample.instance, i) {
	inds <- resample.instance["test.inds", i]
	return(learn.task["targets", inds])
}


