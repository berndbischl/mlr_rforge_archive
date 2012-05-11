#' Defines a classification task for a given data set.
#' The target variable is converted to a factor if it is a logical, integer or character vector. 
#' 
#' @title Construct classification task.
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
#' @param positive [\code{character(1)}]\cr   
#'   Positive class for binary classification. 
#'   Default is the first factor level of the target attribute. 
#' @param check.data [\code{logical(1)}]
#'   Should sanity of data be checked initially at task creation? 
#'   You should have good reasons to turn this off.
#'   Default is \code{TRUE}
#' @return \code{\link{LearnTask}}.
#' @export
makeClassifTask = function(id, data, target, exclude=character(0), weights=numeric(0), 
  blocking=factor(c()), positive=as.character(NA), check.data=TRUE) {
  
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
  checkArg(positive, "character", len=1, na.ok=TRUE)
  checkArg(check.data, "logical", len=1, na.ok=FALSE)
  
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
  
  levs = levels(as.factor(data[, target]))
  m = length(levs)
  if (is.na(positive)) {
    if (m <= 2)
      positive = levs[1]
  } else {
    if (m > 2)
      stop("Cannot set a positive class for a multiclass problem!")
    if (!(positive %in% levs))
      stop(paste("Trying to set a positive class", positive, "which is not a value of the target variable:", paste(levs, collapse=",")))
  } 
  td = new("TaskDesc", data, target, "classif", id, 
    length(weights) > 0, length(blocking) > 0, positive)      
  
  new("ClassifTask", id=id, target=target, data=data, weights=weights, blocking=blocking, positive=positive)
}

   
print.ClassifTask = function(x, ...) {
  td = object@desc
  di = table(getTargets(object)) 
  di = paste(capture.output(di)[-1], collapse="\n")
  m = length(td@class.levels)
  data = getData(object)
  feat = printToChar(td@n.feat)
  cat(
    "Classification problem ", td@id, "\n",
    "Features:\n", feat, "\n", 
    "Observations: ", td@size , "\n",
    "Missings: ", td@has.missing, "\n", 
    "Infinites: ", td@has.inf, "\n", 
    "Target: ", td@target, "\n", 
    "Classes: ", m, "\n",
    di, "\n",
    ifelse(m == 2, paste("Positive class:", td@positive, "\n"), ""),
    "Has weights: ", td@has.weights, "\n", 
    "Has blocking: ", td@has.blocking, "\n",
    sep = ""
  )
}
