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
				type = "character"
		)
)



#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title classif.task constructor

setMethod(
		f = "initialize",
		signature = signature("classif.task"),
		def = function(.Object, wrapped.learner, data, weights=rep(1, nrow(data)), formula, type = "class") {
			
			
			#todo: check for classif. learner
			
			if (missing(wrapped.learner))
				return(.Object)
			
			.Object@type <- type

			callNextMethod(.Object, 
					check.function = check.task.classif, 
					wrapped.learner = wrapped.learner, 
					data=data,	
					weights=weights,
					formula=formula
			)
		}
)

#' Getter.
#' @param x classif.task object
#' @param i [character]
#' \describe{
#'   \item{class.levels}{All possible class values.}
#'   \item{class.nr}{Number of different classes.}
#' }
#' @seealso [,learn.task-class

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
			wl <- x@wrapped.learner
			return(
					paste(
							"Classification task for ", wl@learner.name, " from package ", wl@learner.pack, "\n\n",
							as.character(x@data.desc), "\n",
							as.character(wl@learner.props), sep=""
					)
			)
		}
)


