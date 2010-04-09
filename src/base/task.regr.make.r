#' @include task.regr.r
roxygen()

#' Defines a regression task for a data set and is the starting point 
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
#' 
#' 
#' @return An object of class \code{\linkS4class{regr.task}}.
#' 
#' @export
#' @rdname make.regr.task
#' 
#' @usage make.regr.task(name, data, target, formula, excluded, weights)
#'
#' @title Contruct regression task.


setGeneric(
		name = "make.regr.task",
		def = function(name, data, target, formula, excluded, weights) {
			if(missing(name))
				name=""
			if (missing(excluded))
				excluded <- character(0)
			if (missing(weights))
				weights = numeric(0)
			standardGeneric("make.regr.task")
		}
)

#' @export


setMethod(
		f = "make.regr.task",
		signature = signature(
				name = "character",
				data = "data.frame", 
				target = "character",
				formula = "missing",
				excluded = "character",
				weights = "numeric" 
		),
		
		def = function(name, data, target, excluded, weights) {
			new("regr.task", name=name, target=target, data=data, excluded=excluded, weights=weights)
		}
)


#' @export

setMethod(
		f = "make.regr.task",
		signature = signature(
				target = "missing",
				formula = "formula",
				data = "data.frame", 
				excluded = "character",
				weights = "numeric" 
		),
		
		def = function(name, data, formula, excluded, weights) {
			data2 <- model.frame(formula, data=data)
			target <- as.character(formula)[2]
			new("regr.task", name=name, target=target, data=data2, excluded=excluded, weights=weights)
		}
)



