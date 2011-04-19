#' @include ClassifTask.R
roxygen()
#' @include RegrTask.R
roxygen()

#' Defines a learning task for a given data set. 
#' The type (classification or regression) is automatically inferred from the target variable.
#' Whether a classification or regression task is created depends on the data type of the target variable. 
#' A factor, logical or character vector produces a classification task (and the vector is converted to a factor), 
#' doubles produce regression tasks. Integer target variables have to be changed manually. 
#' 
#' @param id [\code{character(1)}]\cr 
#'   Id string for object. Used to select the object from a named list, etc. Default is the name of the passed R variable.  
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
#' 
#' 
#' @return \code{\linkS4class{LearnTask}}.
#' 
#' @export
#' @rdname make.task
#' 
#' @title Construct learning task.

setGeneric(
  name = "makeMultiLabelTask",
  def = function(id, data, target, exclude, weights, blocking) {
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
    standardGeneric("make.task")
  }
)


#' @export
setMethod(
  f = "makeMultiLabelTask",
  
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
    
    cns = colnames(data)
    x = duplicated(cns)
    if(any(x))
      stop("Duplicated column names in data.frame are not allowed: ", paste(cns[x], collapse=","))
    if (!(target %in% cns)) {
      stop(paste("Column names of data.frame don't contain target var: ", target))
    }
    
    if (any(is.na(data[, target]))) {
      stop("Target values contain missings!")
    }
    if (any(is.infinite(data[, target]))) {
      stop("Target values contain infinite values!")
    }
    
    if (!all(exclude %in% cns))
      stop("Trying to exclude non-existing variables: ", setdiff(exclude, cns))
    if (target %in% exclude)
      stop("Trying to exclude target variable!")
    
    if (length(exclude) > 0)
      data = data[, setdiff(colnames(data), exclude)]
    
    new("MultiLabelTask", id=id, target=target, data=data, weights=weights, blocking=blocking)
  }
)




