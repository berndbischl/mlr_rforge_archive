#' @include data.desc.r
#' @include wrapped.learner.r
roxygen()

#' A learning task is the general description object for a machine learning experiment. 
#' It mainly includes the type of the learning task (e.g. lda), 
#' a dataframe and a formula. As this is just an abstract base class, 
#' you should not instantiate it directly but use the inheriting classes and their factory methods.
#' 
#' @slot wrapped.learner Object of class \code{\linkS4class{wrapped.learner}}.
#' @slot data Dataframe which includes all the data for the task.
#' @slot weights An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @slot formula A symbolic description of the model to be fitted.
#' @slot data.desc Contains logical values describing properties of the dataframe e.g. whether it has 
#' 		characters or missing values (see desc and \code{\linkS4class{data.desc}}).
#' 
#' @exportClass learn.task
#' @seealso classif.task regr.task
#' @title learn.task


setClass(
		"learn.task",
		representation = representation(
				wrapped.learner = "wrapped.learner",
				data = "data.frame",
				weights = "numeric",
				formula = "formula",
				data.desc = "data.desc" 
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title learn.task constructor

setMethod(
		f = "initialize",
		signature = signature("learn.task"),
		def = function(.Object, check.function, wrapped.learner, data, weights, formula) {
			
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			if(missing(wrapped.learner))
				return(.Object)					
			
			.Object@wrapped.learner <- wrapped.learner
			.Object@data <- data
			.Object@weights <- weights
			.Object@formula <- formula
			tn <- .Object["target.name"]
			if (!(tn %in% colnames(data))) {
				stop(paste("Colimn names of data.frame don't contain target var: ", tn))
			}
			
			.Object@data.desc <- make.data.desc(data=data, target.col=tn)
			
			check.result <- check.function(.Object)
			if (check.result$msg != "") {
				stop(check.result$msg)
			}
			else {
				.Object@data <- check.result$data
				.Object@data.desc <- make.data.desc(data=.Object@data, target.col=tn)
			}
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
#' @aliases learn.task-getter 
#' @title Getter for learn.task

setMethod(
		f = "[",
		signature = signature("learn.task"),
		def = function(x,i,j,...,drop) {
			if (i == "target.name"){
				return(as.character(x@formula)[2])
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
				return(attr(terms(x@formula, data=x@data), "term.labels"))
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


#' Set a parameter for the underlying train function of a wrapped learner. 
#' This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
#' fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.
#'   
#' @param object [\code{\linkS4class{learn.task}}] \cr
#'   	Learn task that contains the wrapped learner.
#' 
#' All additional arguments have to be named.
#' 
#' @return learn.task object with changed parameters for train function of the wrapped learner.
#' 
#' @usage set.train.par(wrapped.learner, ...)
#'
#' @title set.train.par

setMethod(
		f = "set.train.par",
		signature = signature("learn.task"),
		def = function(object, ...) {
			object@wrapped.learner <- set.train.par(object@wrapped.learner, ...) 
			return(object) 
		}
)


#' Set a parameter for the underlying predict function of a wrapped learner. 
#' Used to fix (somewhat technical) arguments which stay the same for the whole experiment. Y
#' You should not have to use this too often.
#'   
#' @param object [\code{\linkS4class{learn.task}}] \cr
#'   	Learn task that conatins the wrapped learner.
#' 
#' All additional arguments have to be named.
#' 
#' @return learn.task object with changed parameters for predict function of the wrapped learner.
#' 
#' @usage set.predict.par(wrapped.learner, ...)
#'
#' @title set.predict.par

setMethod(
		f = "set.predict.par",
		signature = signature("learn.task"),
		def = function(object, ...) {
			object@wrapped.learner <- set.predict.par(object@wrapped.learner, ...) 
			return(object) 
		}
)


