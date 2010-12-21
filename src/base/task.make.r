#' @include task.classif.r
#' @include task.regr.r
roxygen()

#' Defines a learning task for a data set and is the starting point 
#' for further steps like training, predicting new data, resampling and tuning and benchmarking.
#' The type (classification or regression) is automatically inferred from the target variable.
#' It might perform some data conversions in the data.frame, like converting integer input features to numerics, 
#' but will generally warn about this. 
#' Whether a classification or regression task is created depends on the data type of the target variable. Factors, logicals or characters
#' produce classification tasks, numerics produce regression tasks. Integer target variables have to be changed manually. 
#' 
#' 
#' @param id [string]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param data [data.frame] \cr 	
#'        A data frame containing the variables for the modeling.
#' @param target [string] \cr
#'  	  Name of the target variable.
#' @param exclude [character]
#'        Names of inputs, which should be generally disregarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [numeric] \cr 	
#'        An optional vector of weights to be used in the fitting process. Default is not to use weights.
#' @param blocking [factor] \cr 	
#'        An optional factor of the same length as the number of observations. Observations with the same blocking level "belong together". Specifically, they are either put all in the training or the test set during a resampling iteration.   
#' @param costs [matrix] \cr 	
#'   An optional matrix of misclassification costs to be used in the fitting process. 
#'   If the used classifier can handle cost matrices it is passed down to its train function. 
#'   Rows indicate true classes, columns predicted classes.
#' 	 Don't pass this in case of regression.	
#' @param positive [string] \cr 	
#'   Positive class for binary classification. Default is the first factor level of the target attribute. 
#' 	 Don't pass this in case of regression.	
#' 
#' 
#' @return \code{\linkS4class{learn.task}}.
#' 
#' @export
#' @rdname make.task
#' 
#' @title Construct learning task.

setGeneric(
  name = "make.task",
  def = function(id, data, target, exclude, weights, blocking, costs, positive) {
    if(missing(id)) {
      id = deparse(substitute(data))
      if (!is.character(id) || length(id) != 1)
        stop("Cannot infer id for task automatically. Please set it manually!")
    }
    if (missing(exclude))
      exclude = character(0)
    if (missing(weights))
      weights = numeric(0)
    else if (is.integer(weights))
      weights = as.numeric(weights)
    if (missing(blocking))
      blocking = factor(c())
    if (missing(costs)) 
      costs = matrix(0,0,0)
    if (missing(positive))
      positive = as.character(NA)
    check.arg(positive, "character", 1)
    standardGeneric("make.task")
  }
)


#' @export
setMethod(
  f = "make.task",
  
  signature = signature(
    id="character", 
    data="data.frame", 
    target="character", 
    exclude="character", 
    weights="numeric", 
    blocking="factor",
    costs="matrix",
    positive="character"
  ),
  
  def = function(id, data, target, exclude, weights, blocking, costs, positive) {
    
    if(length(weights) > 0 && length(weights) != nrow(data))
      stop("Weights have to be of the same length as number of rows in data! Or pass none at all.")
    if(length(blocking) > 0 && length(blocking) != nrow(data))
      stop("Blockings have to be of the same length as number of rows in data! Or pass none at all.")
    
    check.task(data, target)
    
    if(is.factor(data[,target]) || is.character(data[,target]) || is.logical(data[,target]))
      type = "classif"
    else if(is.numeric(data[,target]) && !is.integer(data[,target]))
      type = "regr"
    else 
      stop("Cannot infer the type of task from the target data type. Please transform it!")
    
    if (type == "classif") {
      new("classif.task", id=id, target=target, data=data, exclude=exclude, weights=weights, blocking=blocking, costs=costs, positive=positive)
    } else {
      if(!is.na(positive))
        stop("You cannot define a positive class for regression!")
      if (!(all(dim(costs) == 0)))
        stop("You cannot define a cost matrix for regression!")
      new("regr.task", id=id, target=target, data=data, exclude=exclude, weights=weights, blocking=blocking)
    }
  }
)




