#' Description object for task.
#'  
#' Description object for task, encapsulates basic statistics 
#' without having to store the complete data set.
#' 
#' \describe{
#' \item{env [\code{environment}]}{type Type of task, either \dQuote{classif} for classification or \dQuote{regr} for regression.}
#' \item{id [\code{environment}]}{Id string of task.}
#' \item{target [\code{environment}]}{Name of target variable.}
#' \item{size[\code{environment}]}{Number of cases.}
#' \item{n.feat [\code{environment}]}{Number of covariates, named vector with entries: \dQuote{numerics}, \dQuote{integers}, \dQuote{factors}, \dQuote{characters}, \dQuote{logicals}.}
#' \item{class.levels [\code{environment}]}{All possible classes. Character vector. NA if not classification.}
#' \item{has.missing [\code{environment}]}{ Are missing values present?}
#' \item{has.inf [\code{environment}]}{Are infinite numerical values present?}
#' \item{env [\code{environment}]}{has.blocking Is blocking available in task for observations?}
#' \item{positive [\code{environment}]}{Positive class label for binary classification, NA else.} 
#' \item{negative [\code{environment}]}{Negative class label for binary classification, NA else.} 
#' }
#' @name TaskDesc
#' @rdname TaskDesc
#' @export
NULL

makeTaskDesc = function(data, target, type, id, has.blocking, positive) {
  td = list()
  td$type = type
  td$id = id
  i = which(colnames(data) %in% c(target))
  td$target = target 
  td$size = nrow(data)
  y = data[, target]
  td$n.feat = c(
    numerics = sum(sapply(data, is.double)) - is.double(y), 
    factors = sum(sapply(data, is.factor)) - is.factor(y)
  )
  td$has.missing = any(sapply(data, function(x) any(is.na(x))))
  td$has.inf = any(sapply(data, function(x) any(is.infinite(x))))
  if(type == "classif")
    td$class.levels = levels(y)
  else
    td$class.levels = as.character(NA)
  td$has.blocking = has.blocking
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