#' @include data.desc.r
#' @include wrapped.learner.r
roxygen()

#' A learning task is the general description object for a machine learning experiment, which contains 
#' all initial setup for a learning task. It mainly includes the type of the learning task (e.g. lda), 
#' a dataframe and a formula. As this is just an abstract base class, 
#' you should not instantiate it directly but rather inherit from it in the learn.task classes of
#' your specific classifiers. 
#' 
#' @slot wrapped.learner Object of class \code{\linkS4class{wrapped.learner}}.
#' @slot data Dataframe which includes all the data for the task.
#' @slot weights An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @slot formula A symbolic description of the model to be fitted.
#' @slot data.desc Contains logical values describing properties of the dataframe e.g. whether it has 
#' 		characters or missing values (see desc and \code{\linkS4class{data.desc}}).
#' @slot resampled Internal usage. Don't access 
#' 
#' @exportClass learn.task
#' @title learn.task




setClass(
		"learn.task",
		representation = representation(
				wrapped.learner = "wrapped.learner",
				data = "data.frame",
				weights = "numeric",
				formula = "formula",
				data.desc = "data.desc", 
				resampled = "numeric"
		)
)


#---------------- constructor---- -----------------------------------------------------


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
			cn <- .Object["target.name"]
			.Object@data.desc <- new("data.desc", data, cn)
			.Object@resampled <- numeric(length=0)
			
			
			check.result <- check.function(.Object)
			if (check.result$msg != "") {
				stop(check.result$msg)
			}
			else {
				.Object@data <- check.result$data
				.Object@data.desc <- new("data.desc", .Object@data, cn)
			}
			return(.Object)
		}
)


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




setMethod(
		f = "print",
		signature = signature("learn.task"),
		def = function(x, ...) {
			cat(as.character(x))
		}
)

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
	learn.task@resampled <- subset
	return(learn.task)
}



