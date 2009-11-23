#' @include task.learn.r
roxygen()

#' General description object for a classification experiment.   
#' Instantiate it by using its factory method.
#' 
#' @slot type "class" if you generally want to predict classes or "prob" for probabilities. Default is "class" 
#' 
#' @exportClass classif.task
#' @title classif.task
#' @seealso make.regr.task 


setClass(
		"classif.task",
		contains = c("learn.task"),
		representation = representation(
				type = "character",
				costs = "matrix"
		)
)



#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title classif.task constructor

setMethod(
		f = "initialize",
		signature = signature("classif.task"),
		def = function(.Object, target, data, weights, costs, type = "class") {
			
			
			if (missing(data))
				return(.Object)
			
			.Object@type <- type
			.Object@costs <- costs
			
			callNextMethod(.Object, data=data, weights=weights,	target=target, prep.fct=prep.classif.data)
		}
)

#' Getter.
#' @param x classif.task object
#' @param i [character]
#' \describe{
#'   \item{class.levels}{All possible class values.}
#'   \item{class.nr}{Number of different classes.}
#' }
#' @rdname getter,classif.task-method
#' @aliases classif.task.getter getter,classif.task-method
#' @seealso \code{\link{getter,learn.task-method}}
#' @title Getter for classif.task

setMethod(
		f = "[",
		signature = signature("classif.task"),
		def = function(x,i,j,...,drop) {

			if (i == "class.levels") {
				return(levels(x["targets"]))
			}
			if (i == "class.nr") {
				return(length(levels(x["targets"])))
			}
			callNextMethod()
		}
)


#' Conversion to string.
setMethod(
		f = "as.character",
		signature = signature("classif.task"),
		def = function(x) {
			return(
					paste(
							"Classification problem\n",
							as.character(x@data.desc), "\n",
							sep=""
					)
			)
		}
)


