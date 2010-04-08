#' @include task.classif.r
roxygen()

#' Defines a classification task for a data set and is the starting point 
#' for further steps like training, predicting new data, resampling and tuning and benchmarking.
#' It might perform some data conversions in the data.frame, like coverting integer features to numerics 
#' or integer classes to factors, but will generally warn about this. 
#' 
#' @param name [string] \cr
#'   	  Name of task to be used string representations later on. Default is empty string.
#' @param data [\code{\link{data.frame}}] \cr 	
#'        A data frame containing the variables for the modelling.
#' @param target [string] \cr
#'  	  Name of the target variable.
#' @param formula [\code{\link{formula}}] \cr
#'        Instead of specifying the target, you can use the formula interface. 
#'        If you are using just a subset of the variables or transformations, this will built a new internal 
#'        data frame by calling \code{\link{model.frame}}.
#' @param excluded [\code{\link{character}}]
#'        Names of inputs, which should be generally disregarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [\code{\link{numeric}}] \cr 	
#'        An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.
#' @param costs [\code{\link{matrix}}] \cr 	
#'        A optional matrix of misclassification costs to be used in the fitting process.
#' @param positive [string] \cr 	
#'        Positive class for binary classification. Default is the first factor level of the target attribute. 
#' 
#' 
#' @return An object of class \code{\linkS4class{classif.task}}.
#' 
#' @export
#' @rdname make.classif.task
#' 
#' @usage make.classif.task(name, data, target, formula, excluded, weights, costs, positive)
#'
#' @title Contruct classification task.


setGeneric(
		name = "make.classif.task",
		def = function(name, data, target, formula, excluded, weights, costs, positive) {
			if(missing(name))
				name=""
			if (missing(excluded))
				excluded = character(0)
			if (missing(weights))
				weights <- rep(1, nrow(data))
			if (missing(costs)) {
				# we set costs in constructor after data preparation
				costs = as.matrix(NA)
			}
			if (missing(positive))
				positive = as.character(NA)
			standardGeneric("make.classif.task")
		}
)


#' @export

setMethod(
		f = "make.classif.task",
		signature = signature(
				name = "character",
				data = "data.frame", 
				target = "character",
				formula = "missing",
				excluded = "character",
				weights = "numeric", 
				costs = "matrix",
				positive = "character" 
		),
		
		def = function(name, data, target, excluded, weights, costs, positive) {
			return(
					new("classif.task", name=name, target=target, data=data, excluded=excluded, weights=weights, costs=costs, positive=positive)
			)
		}
)

#' @export

setMethod(
		f = "make.classif.task",
		signature = signature(
				name = "character",
				data = "data.frame", 
				target = "missing",
				formula = "formula",
				excluded = "character",
				weights = "numeric", 
				costs = "matrix", 
				positive = "character" 
		),
		
		def = function(name, data, formula, excluded, weights, costs, positive) {
			data2 <- model.frame(formula, data=data)
			target <- as.character(formula)[2]
			return(
				new("classif.task", name=name, target=target, data=data2, excluded=excluded, weights=weights, costs=costs, positive=positive)
			)	
		}
)




