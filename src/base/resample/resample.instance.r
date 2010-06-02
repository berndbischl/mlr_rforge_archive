#' @include resample.desc.r
roxygen()

#' Base class for specific resampling draws like cross-validation or bootstrapping.
#' This class encapsulates training and test sets generated from the data set for a number of iterations. 
#' It mainly stores a set of integer vectors indicating the training examples for each iteration.
#' Don't create objects from this class directly but use the corresponding subclasses.
#' For construction simply use the factory method \code{\link{make.res.instance}}. 
#' 
#' @note If you want to add another resampling strategy, have a look at the web documentation. 
#' @exportClass resample.instance
#' @seealso \code{\linkS4class{resample.desc}}, \code{\link{make.res.instance}}, \code{\link{resample.fit}} 
#' @title Resampling instance.


# todo validation for size
setClass(
		"resample.instance",   
		contains = c("object"), 
		# we always have to store training inds because the order might matter
		representation = representation(
				desc = "resample.desc", 
				size = "integer", 
				inds = "list"
		)
)


#' Constructor.

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



#setGeneric(
#		name = "make.resample.instance",
#		def = function(desc, size) {
#			standardGeneric("make.resample.instance")
#		}
#)




#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("resample.instance"),
		def = function(x) {
			return(
					paste(
							"Instance for ", x@desc@name,  " with ", length(x@inds), " iterations and ", x@size, " cases\n",
							paste(capture.output(str(x@inds)), collapse="\n"), 
							"\n", sep=""
					)
			)
		}
)



#----------------- getter ---------------------------------------------------------

#' Getter.
#' 
#' \describe{
#' 	\item{size [integer]}{Number of observations.}
#' 	\item{name [character}}{The name of the resample description object, i.e. the type of resampling.}
#' 	\item{iters [integer]}{The number of resampling iterations.}
#'  \item{train.inds [list | integer] Optional parameter: iteration index j}{If j is a single integer, the vector of training indices for the jth iteration is returned. If j is an integer vector, the list of training indices for the given iterations is returned. If j is missing, all indices are returned.}
#'  \item{test.inds [list | integer] Optional parameter: iteration index j}{If j is a single integer, the vector of test indices for the jth iteration is returned. If j is an integer vector, the list of test indices for the given iterations is returned. If j is missing, all indices are returned.}
#' }
#' 
#' @rdname resample.instance-class

setMethod(
		f = "[",
		signature = signature("resample.instance"),
		def = function(x,i,j,...,drop) {
			if (i == "size")
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
				size <- x["size"]
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


