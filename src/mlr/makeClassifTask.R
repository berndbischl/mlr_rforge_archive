#' @include ClassifTask.R
roxygen()

#' Defines a classification task for a given data set. 
#' It might perform some data conversions in the data.frame, like converting integer input features to doubles, 
#' but will generally warn about this. If you want to change default preprocessing behaviour, look at
#' of \code{makeClassifTask}.
#' The target variable is converted to a factor if it is a logical, integer or character vector. 
#' 
#' @param id [\code{character(1)}]\cr 
#'   Id string for object. Used to select the object from a named list, etc. Default is the name of R variable passed to \code{data}.  
#' @param data [\code{data.frame}] \cr   
#'   A data frame containing the input and target variables for modeling.
#' @param target [\code{character(1)}] \cr
#'   Name of the target variable.
#' @param exclude [character]
#'   Names of inputs, which should be discarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [numeric] \cr   
#'   An optional vector of case weights to be used in the fitting process (if the learner cannot handle weights, they are ignored). Default is not to use weights.
#' @param blocking [factor] \cr   
#'   An optional factor of the same length as the number of observations. Observations with the same blocking level "belong together". Specifically, they are either put all in the training or the test set during a resampling iteration.
#' @param positive [\code{character(1)}] \cr   
#'   Positive class for binary classification. Default is the first factor level of the target attribute. 
#' @param check.data [\code{logical(1)}]
#'   Should sanity of data be checked initially at task creation? You should have good reasons to turn this off...
#' 
#' @return \code{\linkS4class{LearnTask}}.
#' 
#' @exportMethod makeClassifTask
#' @rdname makeClassifTask
#' 
#' @title Construct learning task.

setGeneric(
  name = "makeClassifTask",
  def = function(id, data, target, exclude, weights, blocking, positive, check.data) {
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
    if (missing(positive))
      positive = as.character(NA)
    check.arg(positive, "character", 1)
    if (missing(check.data))
      check.data = TRUE
    check.arg(check.data, "logical", 1)
    standardGeneric("makeClassifTask")
  }
)


#' @rdname makeClassifTask
setMethod(
  f = "makeClassifTask",
  
  signature = signature(
    id="character", 
    data="data.frame", 
    target="character", 
    exclude="character", 
    weights="numeric", 
    blocking="factor",
    positive="character"
  ),
  
  def = function(id, data, target, exclude, weights, blocking, positive, check.data) {
    
    checkWeightsAndBlocking(data, target, weights, blocking)    
    checkColumnNames(data, target, exclude)
    if (!is.factor(data[, target])) {
      warning("Converting target to factor.")
      data[, target] = as.factor(data[, target])
    }
    if (length(exclude) > 0)
      data = data[, setdiff(colnames(data), exclude)]
    if (check.data)
      checkData(data, target)    
    
    new("ClassifTask", id=id, target=target, data=data, weights=weights, blocking=blocking, positive=positive)
  }
)




