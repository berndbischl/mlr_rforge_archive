#' @include resample.desc.r
roxygen()

#' Base class for specific resampling draws like cross-validation or bootstrapping.
#' This class encapsulates training and test sets generated from the data set for a number of iterations. 
#' It mainly stores a set of integer vectors indicating the training examples for each iteration.
#' Don't create objects from this class directly but use the corresponding subclasses.
#' For construction simply use the factory methods of the subclasses - e.g. for cross-validation 
#' use \code{\link{make.cv.instance}} to get a \code{\linkS4class{cv.instance}}.  
#' 
#' @slot desc Description object for resampling strategy
#' @slot size Number of observations in data
#' @slot inds List of integer vectors specifying the training cases for each iteration. Each vector might contain duplicated indices and the order matters for some classifiers.
#' 
#' @note If you want to add another resampling strategy, have a look at the web documentation. 
#' @exportClass resample.instance
#' @seealso \code{\linkS4class{resample.desc}}, \code{\link{make.cv.instance}}, \code{\link{make.bs.instance}}, \code{\link{make.subsample.instance}}, \code{\link{resample.fit}} 
#' @title resample.instance


# todo validation for size
setClass(
		"resample.instance",                                                     
		# we always have to store training inds because the order might matter
		representation = representation(
				desc = "resample.desc", 
				size = "integer", 
				inds = "list"
		)
)



#' This is mainly for internal use, you only need to use this, when you extend resample.instance to add another resampling strategy!
#'  
#' @param desc [\code{\linkS4class{resample.desc}}] \cr resample.desc. Describes the resampling strategy.
#' @param size [integer] \cr Size of the data set to resample from.
#' @param inds [list of integer vectors] \cr Indices of the trainings sets.
#' @rdname resample.instance-class

setMethod(
		f = "initialize",
		signature = signature("resample.instance"),
		def = function(.Object, desc, size, inds) {
			if (missing(desc))
				return(.Object)
			.Object@desc <- desc
			if (round(size) != size)
				error("You passed a non-integer to arg 'size' of resample.instance!")
			.Object@size <- as.integer(size)
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

#' Mainly for internal use. Construct a \code{\linkS4class{resample.instance}} from a \code{\linkS4class{resample.desc}}. 
#' Convenience method, so one doesn't have to call the specific constructors of classes inheriting from resample.desc.
#' 
#' @param desc [\code{\linkS4class{resample.desc}}] \cr Describes the resampling strategy.
#' @param size [integer] \cr Size of the data set to resample from. 
#'              
#' @return Object of corresponding subclass of \code{\linkS4class{resample.instance}.
#' @export
#' @rdname make.resample.instance
#'	
#' @usage make.resample.instance(desc, size) 
#'
#' @examples 
#'   cv.d <- new("cv.desc", folds = 10)
#'   rin <- make.resample.instance(desc = cv.d, size = nrow(iris))

setMethod(
		f = "make.resample.instance",
		signature = signature(desc = "resample.desc", size = "numeric"),
		def = function(desc, size) {
			return(new(desc@instance.class, desc, size = size))
		}
)



#----------------- getter ---------------------------------------------------------

#' Getter.
#' @param x resample.instance object
#' @param i [character]
#' 	\item{data.size}{The size of the dataframe.}
#' 	\item{name}{The name of the resample description object, i.e. the type of resampling.}
#' 	\item{iters}{The number of resampling iterations.}
#'  \item{train.inds}{If j is a single integer, the vector of training indices for the jth iteration is returned. If j is an integer vector, the list of training indices for the given iterations is returned. If j is missing, all indices are returned.}
#'  \item{test.inds}{If j is a single integer, the vector of test indices for the jth iteration is returned. If j is an integer vector, the list of test indices for the given iterations is returned. If j is missing, all indices are returned.}
#' @param j [integer] \cr See above, i == "train.inds" or i == "test.inds".
#' @rdname resample.instance-class


setMethod(
		f = "[",
		signature = signature("resample.instance"),
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


