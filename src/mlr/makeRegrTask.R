#' @include task.classif.r
roxygen()
#' @include task.regr.r
roxygen()
#' @include prepare.df.r
roxygen()

#' Defines a regression task for a given data set. 
#' It might perform some data conversions in the data.frame, like converting integer input features to doubles, 
#' but will generally warn about this. If you want to change default preprocessing behaviour, look at
#' \code{\link{prepare.control}}, construct the control object yourself and pass it into the \code{control} argument 
#' of \code{makeRegrTask}.
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
#' 
#' 
#' @return \code{\linkS4class{LearnTask}}.
#' 
#' @export
#' @rdname makeRegrTask
#' 
#' @title Construct learning task.

setGeneric(
  name = "makeRegrTask",
  def = function(id, data, target, exclude, weights, blocking, control) {
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
    if (missing(control))
      control = prepare.control()
    standardGeneric("makeRegrTask")
  }
)


#' @export
setMethod(
  f = "makeRegrTask",
  
  signature = signature(
    id="character", 
    data="data.frame", 
    target="character", 
    exclude="character", 
    weights="numeric", 
    blocking="factor",
    control="prepare.control"
  ),
  
  def = function(id, data, target, exclude, weights, blocking, control) {
    checkWeightsAndBlocking(data, target, weights, blocking)    
    checkColumnNames(data, target, exclude)
    if (length(exclude) > 0)
      data = data[, setdiff(colnames(data), exclude)]
    checkData(data, target, exclude)    
    
    
    data = prep.data("regr", data, target, control)      
    new("RegrTask", id=id, target=target, data=data, weights=weights, blocking=blocking, control=control)
  }
)




