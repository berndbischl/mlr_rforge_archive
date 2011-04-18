#' @include RegrTask.R
roxygen()

#' Defines a regression task for a given data set. 
#' It might perform some data conversions in the data.frame, like converting integer
#' 
#' @param id [character(1)]\cr 
#'   Id string for object. Used to select the object from a named list, etc. Default is the name of R variable passed to \code{data}.  
#' @param data [data.frame] \cr   
#'   A data frame containing the input and target variables for modeling.
#' @param target [character(1)] \cr
#'   Name of the target variable.
#' @param exclude [character]
#'   Names of inputs, which should be discarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [numeric] \cr   
#'   An optional vector of case weights to be used in the fitting process (if the learner cannot handle weights, they are ignored). Default is not to use weights.
#' @param blocking [factor] \cr   
#'   An optional factor of the same length as the number of observations. Observations with the same blocking level "belong together". Specifically, they are either put all in the training or the test set during a resampling iteration.
#' @param check.data [\code{logical(1)}]
#'   Should sanity of data be checked initially at task creation? You should have good reasons to turn this off...
#' 
#' @return \code{\linkS4class{LearnTask}}.
#' 
#' @exportMethod makeRegrTask
#' @rdname makeRegrTask
#' 
#' @title Construct learning task.

setGeneric(
  name = "makeRegrTask",
  def = function(id, data, target, exclude, weights, blocking, check.data) {
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
    if (missing(check.data))
      check.data = TRUE
    check.arg(check.data, "logical", 1)
    standardGeneric("makeRegrTask")
  }
)

#' @rdname makeRegrTask
setMethod(
  f = "makeRegrTask",
  
  signature = signature(
    id="character", 
    data="data.frame", 
    target="character", 
    exclude="character", 
    weights="numeric", 
    blocking="factor"
  ),
  
  def = function(id, data, target, exclude, weights, blocking, check.data) {
    checkWeightsAndBlocking(data, target, weights, blocking)    
    checkColumnNames(data, target, exclude)
    if (!is.double(data[, target])) {
      warning("Converting target to numeric.")
      data[, target] = as.numeric(data[, target])
    }
    if (length(exclude) > 0)
      data = data[, setdiff(colnames(data), exclude)]
    if (check.data)
      checkData(data, target)    
    
    new("RegrTask", id=id, target=target, data=data, weights=weights, blocking=blocking)
  }
)




