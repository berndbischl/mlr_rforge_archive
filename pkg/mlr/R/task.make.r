#' @include task.classif.r
#' @include task.regr.r
roxygen()

#' Defines a learning task for a data set and is the starting point 
#' for further steps like training, predicting new data, resampling and tuning and benchmarking.
#' The type (classification or regression) is automatically inferred from the target variable.
#' It might perform some data conversions in the data.frame, like converting integer input features to numerics, 
#' but will generally warn about this. 
#' Whether a classification or regression task is created depends on the data type of the target variable. Factors or characters
#' produce classification tasks, numerics produce regression tasks. Integer target variables have to be changed manually. 
#' 
#' 
#' @param id [string]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param label [string]\cr 
#'        Label string for object. Used in plots, etc.  
#' @param data [\code{\link{data.frame}}] \cr 	
#'        A data frame containing the variables for the modeling.
#' @param target [string] \cr
#'  	  Name of the target variable.
#' @param excluded [\code{\link{character}}]
#'        Names of inputs, which should be generally disregarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [\code{\link{numeric}}] \cr 	
#'        An optional vector of weights to be used in the fitting process. Default is not to use weights.
#' @param costs [\code{\link{matrix}}] \cr 	
#'        An optional matrix of misclassification costs to be used in the fitting process.
#' 		  Ignored for regression.	
#' @param positive [string] \cr 	
#'        Positive class for binary classification. Default is the first factor level of the target attribute. 
#' 		  Ignored for regression.	
#' 
#' 
#' @return \code{\linkS4class{learn.task}}.
#' 
#' @export
#' @rdname make.task
#' 
#' @usage make.task(id, label, data, target, excluded, weights, costs, positive)
#'
#' @title Construct learning task.


make.task = function(id, label, data, target, excluded, weights, costs, positive) {
			if(missing(id)) {
				id = deparse(substitute(data))
				if (!is.character(id) || length(id) != 1)
					stop("Cannot infer id for task automatically. Please set it manually!")
			}
			if(missing(label))
				label = id
			
			check.task(data, target)
			
			if(is.factor(data[,target]) || is.character(data[,target]))
				type = "classif"
			else if(is.numeric(data[,target]) && !is.integer(data[,target]))
				type = "regr"
			else 
				stop("Cannot infer the type of task from the target data type. Please transform it!")
			
			if (missing(excluded))
				excluded = character(0)
			if (missing(weights))
				weights = numeric(0) 
			if (missing(costs)) {
				costs = matrix(0,0,0)
			}
			if (missing(positive) && type == "classif")
				positive = as.character(NA)
			
			if (type == "classif") {
				new("classif.task", id=id, label=label, target=target, data=data, excluded=excluded, weights=weights, costs=costs, positive=positive)
			} else {
				new("regr.task", id=id, label=label, target=target, data=data, excluded=excluded, weights=weights)
			}
}

