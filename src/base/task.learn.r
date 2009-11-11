#' @include data.desc.r
roxygen()

#' A learning task is a general description object for a machine learning experiment. 
#' It wraps the data source and specifies - through its subclasses - the type of the task (e.g. classification or regression), 
#' the target variable, the loss function and other details of the problem. As this is just an abstract base class, 
#' you should not instantiate it directly but use the inheriting classes and their factory methods.
#' 
#' @slot data Dataframe which includes all the data for the task.
#' @slot weights An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @slot target Name of the target variable.
#' @slot data.desc Contains logical values describing properties of the dataframe e.g. whether it has 
#' 		characters or missing values (see desc and \code{\linkS4class{data.desc}}).
#' 
#' @exportClass learn.task
#' @seealso classif.task regr.task
#' @title learn.task


setClass(
		"learn.task",
		representation = representation(
				data = "data.frame",
				target = "character",
				data.desc = "data.desc", 
				weights = "numeric"
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title learn.task constructor

setMethod(
		f = "initialize",
		signature = signature("learn.task"),
		def = function(.Object, data, target, weights, prep.fct) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			if(missing(data))
				return(.Object)					
			
			.Object@data <- prep.fct(data, target)
			.Object@weights <- weights
			.Object@target <- target
			if (!(target %in% colnames(data))) {
				stop(paste("Column names of data.frame don't contain target var: ", tn))
			}
			.Object@data.desc <- make.data.desc(data, target)
			
			return(.Object)
		}
)

#' Getter.
#' @param x learn.task object
#' @param i [character]
#' \describe{
#'   \item{target.name}{The name of the target variable.}
#'   \item{target.col}{The column number of the target variable.}
#'   \item{targets}{If j is missing all target values are returned. Otherwise they are indexed by j.}
#'   \item{input.names}{The names of the input variables.}
#' }
#' @param j [integer] \cr See above, i == "targets".
#' 
#' @rdname getter,learn.task-method
#' @aliases learn.task.getter getter,learn.task-method
#' @title Getter for learn.task

setMethod(
		f = "[",
		signature = signature("learn.task"),
		def = function(x,i,j,...,drop) {
			if (i == "target.name"){
				return(x@target)
			}
			if (i == "target.col"){
				return(which(colnames(x@data) == x["target.name"]))
			}
			
			if (i == "targets") {
				if (missing(j))
					j = 1:nrow(x@data)
				return(x@data[j, x["target.name"]])
			}
			if (i == "input.names"){
				return(setdiff(colnames(x@data), x["target.name"]))
			}
			
			#if nothing special return slot
			return(
					eval(substitute("@"(x, slot), list(slot=i)))
			)
		}
)



#' Prints the object by calling as.character.
setMethod(
		f = "print",
		signature = signature("learn.task"),
		def = function(x, ...) {
			cat(as.character(x))
		}
)

#' Shows the object by calling as.character.
setMethod(
		f = "show",
		signature = signature("learn.task"),
		def = function(object) {
			cat(as.character(object))
		}
)

#---------------- restrict.learn.task -----------------------------------------------------

restrict.learn.task <- function(learn.task, subset) {
	learn.task@data <- learn.task@data[subset,]
	return(learn.task)
}


