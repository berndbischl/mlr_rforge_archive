#' @include task.classif.r
roxygen()
#' @include task.regr.r
roxygen()
#' @include prepare.df.r
roxygen()

#' Defines a classification task for a given data set. 
#' The type (classification or regression) is automatically inferred from the target variable.
#' It might perform some data conversions in the data.frame, like converting integer input features to doubles, 
#' but will generally warn about this. If you want to change default preprocessing behaviour, look at
#' \code{\link{prepare.control}}, construct the control object yourself and pass it into the \code{control} argument 
#' of \code{makeClassifTask}.
#' Whether a classification or regression task is created depends on the data type of the target variable. 
#' A factor, logical or character vector produces a classification task (and the vector is converted to a factor), 
#' doubles produce regression tasks. Integer target variables have to be changed manually. 
#' 
#' @param id [string]\cr 
#'   Id string for object. Used to select the object from a named list, etc. Default is the name of the passed R variable.  
#' @param data [data.frame] \cr   
#'   A data frame containing the input and target variables for modeling.
#' @param target [string] \cr
#'   Name of the target variable.
#' @param exclude [character]
#'   Names of inputs, which should be discarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [numeric] \cr   
#'   An optional vector of case weights to be used in the fitting process (if the learner cannot handle weights, they are ignored). Default is not to use weights.
#' @param blocking [factor] \cr   
#'   An optional factor of the same length as the number of observations. Observations with the same blocking level "belong together". Specifically, they are either put all in the training or the test set during a resampling iteration.
#' @param control [\code{\linkS4class{prepare.control}}] \cr  
#'   Optional control object used for preparing the data.frame. For defaults look at \code{\link{prepare.control}}.
#' @param costs [matrix] \cr  
#'   An optional matrix of misclassification costs to be used in the fitting process. 
#'   If the used classifier can handle cost matrices it is passed down to its train function, otherwise it is ignored.
#'   Rows indicate true classes, columns predicted classes.
#'   Don't pass this in case of regression. 
#' @param positive [string] \cr   
#'   Positive class for binary classification. Default is the first factor level of the target attribute. 
#'   Don't pass this in case of regression. 
#' 
#' 
#' @return \code{\linkS4class{LearnTask}}.
#' 
#' @export
#' @rdname makeClassifTask
#' 
#' @title Construct learning task.

setGeneric(
  name = "makeClassifTask",
  def = function(id, data, target, exclude, weights, blocking, control, costs, positive) {
    if(missing(id)) {
      id = deparse(substitute(data))
      if (!is.character(id) || length(id) != 1)
        stop("Cannot infer id for task automatically. Please set it manually!")
    }
    if (missing(exclude))
      exclude = character(0)
    if (missing(weights)) {
      weights = numeric(0)
    } else {
      if (is.integer(weights))
        weights = as.numeric(weights)
      check.arg(weights, "numeric", nrow(data))
    }
    if (missing(blocking))
      blocking = factor(c())
    else 
      check.arg(blocking, "factor", nrow(data))
    if (missing(costs)) 
      costs = matrix(0,0,0)
    check.arg(costs, "matrix")
    if (missing(positive))
      positive = as.character(NA)
    check.arg(positive, "character", 1)
    if (missing(control))
      control = prepare.control()
    standardGeneric("makeClassifTask")
  }
)


#' @export
setMethod(
  f = "makeClassifTask",
  
  signature = signature(
    id="character", 
    data="data.frame", 
    target="character", 
    exclude="character", 
    weights="numeric", 
    blocking="factor",
    control="prepare.control",
    costs="matrix",
    positive="character"
  ),
  
  def = function(id, data, target, exclude, weights, blocking, control, costs, positive) {
    
    checkWeightsAndBlocking(data, target, weights, blocking)    
    checkColumnNames(data, target, exclude)
    if (length(exclude) > 0)
      data = data[, setdiff(colnames(data), exclude)]
    checkData(data, target, exclude)    
    
    data = prep.data(TRUE, data, target, control)      
    
    new("ClassifTask", id=id, target=target, data=data, weights=weights, blocking=blocking, control=control, costs=costs, positive=positive)
  }
)




