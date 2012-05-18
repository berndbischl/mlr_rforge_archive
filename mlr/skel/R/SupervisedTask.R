#' Create a classification / regression task for a given data set.
#' 
#' The task encapsulates the data and specifies - through its subclasses - the type of the task (either classification or regression), 
#' and contains a description object detailing further aspects of the data. 
#' 
#' Classification: The target variable is converted to a factor if it is a 
#' logical, integer or character vector.
#' Regression: The target variable is converted to a numeric.
#' 
#' Object slots:
#' \describe{
#' \item{env [\code{environment}]}{Environment where data for the task are stored. Use \code{\link{getTaskData}} in order to access it.}
#' \item{blocking [\code{factor}]}{See argument above.}
#' \item{desc [\code{\link{TaskDesc}}]}{Encapsulates further information about the task. See class documentation.}
#' }
#' 
#' @param id [\code{character(1)}]\cr 
#'   Id string for object. 
#'   Default is the name of R variable passed to \code{data}.  
#' @param data [\code{data.frame}]\cr   
#'   A data frame containing the input and target variables.
#' @param target [\code{character(1)}]\cr
#'   Name of the target variable.
#' @param exclude [\code{character}]
#'   Names of features which should be discarded, e.g. IDs, etc. 
#'   Default is none. 
#' @param blocking [\code{factor}]\cr
#'   An optional factor of the same length as the number of observations. 
#'   Observations with the same blocking level \dQuote{belong together}. 
#'   Specifically, they are either put all in the training or the test set 
#'   during a resampling iteration.
#'   Default is \code{factor(c())} which means no blocking. 
#' @param positive [\code{character(1)}]\cr   
#'   Positive class for binary classification. 
#'   Default is the first factor level of the target attribute. 
#' @param check.data [\code{logical(1)}]
#'   Should sanity of data be checked initially at task creation? 
#'   You should have good reasons to turn this off.
#'   Default is \code{TRUE}
#' @return \code{\link{SupervisedTask}}.
#' @export
#' @rdname SupervisedTask
#' 
#'  
#' @seealso \code{\link{makeClassifTask}}, \code{\link{makeRegrTask}}
#' @name SupervisedTask
NULL

makeSupervisedTask = function(type, id, data, target, exclude, blocking) {
  
  if(missing(id)) {
    id = deparse(substitute(data))
    if (!is.character(id) || length(id) != 1)
      stop("Cannot infer id for task automatically. Please set it manually!")
  } else {
    checkArg(id, "character", len=1, na.ok=FALSE)
  }
  checkArg(data, "data.frame")
  checkArg(target, "character", len=1, na.ok=FALSE)
  checkArg(exclude, "character", na.ok=FALSE)
  if (!identical(blocking, factor(c())))
    checkArg(blocking, "factor", len=nrow(data), na.ok=FALSE)
  checkArg(positive, "character", len=1, na.ok=TRUE)
  checkArg(check.data, "logical", len=1, na.ok=FALSE)
  checkBlocking(data, target, blocking)    
  checkColumnNames(data, target, exclude)
  if (type == "classif") {
    if (!is.factor(data[, target]))
      data[, target] = as.factor(data[, target])
  }
  if (type == "regr") {
    if (!is.double(data[, target]))
      data[, target] = as.numeric(data[, target])
  }
  if (length(exclude) > 0)
    data = data[, setdiff(colnames(data), exclude)]
  if (check.data)
    checkData(data, target)    
  
  env = new.env()
  env$data = data
  structure(list(
    env = env,
    blocking = blocking
  ), class="SupervisedTask")
}

print.SupervisedTask = function(x, ...) {
  td = x$desc
  feat = printToChar(td$n.feat)
  cat(
    "Supervised task: ", td$id, "\n",
    "Type: ", td$type, "\n",
    "Features:\n", feat, "\n", 
    "Observations: ", td$size , "\n",
    "Missings: ", td$has.missing, "\n", 
    "Target: ", td$target, "\n", 
    "Has blocking: ", td$has.blocking, "\n",
    sep=""
  )
}