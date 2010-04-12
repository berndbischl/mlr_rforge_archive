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
#' @rdname make.task
#' 
#' @usage make.task(name, data, target, formula, excluded, weights)
#'
#' @title Contruct regression task.


setGeneric(
		name = "make.task",
		def = function(name, data, target, formula, excluded, weights) {
