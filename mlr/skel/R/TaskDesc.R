#' Description object for task.
#'  
#' Description object for task, encapsulates basic statistics 
#' without having to store the complete data set.
#' 
#' Object members: 
#' \describe{
#' \item{id [\code{character(1)}]}{Id string of task.}
#' \item{type [\code{character(1)}]}{type Type of task, either \dQuote{classif} for classification or \dQuote{regr} for regression.}
#' \item{target [\code{character(1)}]}{Name of target variable.}
#' \item{size[\code{integer(1)}]}{Number of cases.}
#' \item{n.feat [\code{integer}]}{Number of features, named vector with entries: \dQuote{numerics}, \dQuote{factors}.}
#' \item{class.levels [\code{character}]}{All possible classes. \code{NA} if not classification.}
#' \item{has.missing [\code{logical(1)}]}{Are missing values present?}
#' \item{has.weights [\code{logical(1)}]}{Are weights available in task for observations?}
#' \item{has.blocking [\code{logical(1)}]}{Is blocking available in task for observations?}
#' \item{positive [\code{character(1)}]}{Positive class label for binary classification, \code{NA} else.} 
#' \item{negative [\code{character(1)}]}{Negative class label for binary classification, \code{NA} else.} 
#' }
#' @name TaskDesc
#' @rdname TaskDesc
NULL

makeTaskDesc = function(type, id, data, target, weights, blocking, positive) {
  td = list()
  td$id = id
  td$type = type
  i = which(colnames(data) %in% c(target))
  td$target = target 
  td$size = nrow(data)
  y = data[, target]
  td$n.feat = c(
    numerics = sum(sapply(data, is.numeric)) - is.numeric(y), 
    factors = sum(sapply(data, is.factor)) - is.factor(y)
  )
  if(type == "classif")
    td$class.levels = levels(y)
  else
    td$class.levels = as.character(NA)
  td$has.missing = any(sapply(data, function(x) any(is.na(x))))
  td$has.weights = length(weights) > 0
  td$has.blocking = length(blocking) > 0
  if (type == "classif") {
    td$positive = positive
    if (length(td$class.levels) == 1)
      td$negative = paste("not_", positive)
    else if(length(td$class.levels) == 2)
      td$negative = setdiff(td$class.levels, positive)
    else
      td$negative = as.character(NA)
  } else { 
    td$positive = as.character(NA)
    td$negative = as.character(NA)
  }
  return(structure(td, class="TaskDesc"))
}