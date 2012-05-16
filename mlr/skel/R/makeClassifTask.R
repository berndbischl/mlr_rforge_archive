#' Defines a classification task for a given data set.
#' 
#' The target variable is converted to a factor if it is a logical, integer or character vector. 
#' 
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
#' @return \code{\link{SupervisedTask}}.
#' @export
makeClassifTask = function(id, data, target, exclude=character(0), 
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
  if (!identical(blocking, factor(c())))
    checkArg(blocking, "factor", len=nrow(data), na.ok=FALSE)
  checkArg(positive, "character", len=1, na.ok=TRUE)
  checkArg(check.data, "logical", len=1, na.ok=FALSE)
  
  checkBlocking(data, target, blocking)    
  checkColumnNames(data, target, exclude)
  if (!is.factor(data[, target])) {
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
  td = makeTaskDesc(data, target, "classif", id, length(blocking) > 0, positive)      
  task = makeSupervisedTask(data, blocking, td)
  class(task) = c("ClassifTask", class(task))
  return(task)
}
   
print.ClassifTask = function(x, ...) {
  di = printToChar(table(getTargets(x)))
  # remove 1st newline
  di = substr(di, 2, nchar(di))
  m = length(x$desc$class.levels)
  print.SupervisedTask(x)
  cat(
    "Classes: ", m, "\n",
    di, "\n",
    "Positive class: ", x$desc$positive, "\n",
    sep = ""
  )
}
