#' Base class for description of resampling algorithms.
#' A description of a resampling algorithm contains all necessary information to provide a resampling.instance, 
#' when given the size of the data set.
#' 
#' Getter.
#' 
#' \describe{
#' 	\item{instance.class [character]}{S4 class name of the corresponding resample.instance}
#' 	\item{name [character]}{Name of this resampling algorithm}
#' 	\item{iters [numeric]}{Number of iterations}
#' } 
#' @exportClass resample.desc 
#' @title resample.desc

# todo validation for size
setClass(
		"resample.desc", 
		contains = c("object"),
		representation = representation(
				instance.class = "character", 
				name = "character", 
				iters = "integer",
				group.iters = "integer",
				props = "list"
		)
)




setMethod(
		f = "initialize",
		signature = signature("resample.desc"),
		def = function(.Object, instance.class, name, iters, group.iters, ...) {
			if (missing(name))
				return(.Object)					
			if (missing(group.iters))
				group.iters = as.integer(NA)				
			.Object@instance.class = instance.class
			.Object@name = name
			.Object@iters = iters
			.Object@group.iters = group.iters
			return(.Object)
		}
)


#' @rdname resample.desc-class

setMethod(
		f = "[",
		signature = signature("resample.desc"),
		def = function(x,i,j,...,drop) {
			if (i == "is.sequential") {
				return(x@props[[i]])
			}
			callNextMethod(x,i,j,...,drop=drop)
		}
)



#' @rdname to.string

setMethod(
		f = "to.string",
		signature = signature("resample.desc"),
		def = function(x) {
			return(
					paste(
							"Description for ", x@name,  " with ", x@iters, " iterations.\n",
							sep=""
					)
			)
		}
)


setClass(
		"resample.desc.seq", 
		contains = c("resample.desc")
)


setClass(
		"resample.desc.nonseq", 
		contains = c("resample.desc")
)



