#' Defines a regression task for a given data set. 
#' The target variable is converted to a numeric.
#' 
#' @title Construct regression task.
#' @param id [\code{character(1)}]\cr 
#'   Id string for object. Used to select the object from a named list, etc. 
#'   Default is the name of R variable passed to \code{data}.  
#' @param data [\code{data.frame}]\cr   
#'   A data frame containing the input and target variables for modeling.
#' @param target [\code{character(1)}]\cr
#'   Name of the target variable.
#' @param exclude [\code{character}]
#'   Names of inputs, which should be discarded, e.g. IDs, etc. 
#'   Default is none. 
#' @param weights [\code{numeric}]\cr   
#'   An optional vector of case weights to be used in the fitting process.
#'   If the learner cannot handle weights, they are ignored.
#'   Default is not to use weights.
#' @param blocking [\code{factor}]\cr   
#'   An optional factor of the same length as the number of observations. Observations with the same blocking level "belong together". 
#'   Specifically, they are either put all in the training or the test set during a resampling iteration.
#' @param check.data [\code{logical(1)}]
#'   Should sanity of data be checked initially at task creation? 
#'   You should have good reasons to turn this off.
#'   Default is \code{TRUE}
#' @return \code{\linkS4class{LearnTask}}.
#' @export
makeRegrTask = function(id, data, target, exclude=character(0), weights=numeric(0), 
  blocking=factor(c()), check.data=TRUE) {
  
  if(missing(id)) {
    id = deparse(substitute(data))
    if (!is.character(id) || length(id) != 1)
      stop("Cannot infer id for task automatically. Please set it manually!")
  }
  
  checkArg(id, "character", len=1, na.ok=FALSE)
  checkArg(data, "data.frame")
  checkArg(target, "character", len=1, na.ok=FALSE)
  checkArg(exclude, "character", na.ok=FALSE)
  if (!identical(weights, numeric(0)))
    checkArg(weights, "numeric", len=nrow(data), na.ok=FALSE)
  if (!identical(blocking, factor(c())))
    checkArg(blocking, "factor", len=nrow(data), na.ok=FALSE)
  checkArg(check.data, "logical", len=1, na.ok=FALSE)
  
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




