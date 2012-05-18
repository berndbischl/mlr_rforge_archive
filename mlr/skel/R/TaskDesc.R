#' Description object for task.
#'  
#' Description object for task, encapsulates basic statistics 
#' without having to store the complete data set.
#' 
#' \describe{
#' \item{id [\code{environment}]}{Id string of task.}
#' \item{type [\code{character(1)}]}{type Type of task, either \dQuote{classif} for classification or \dQuote{regr} for regression.}
#' \item{target [\code{character(1)}]}{Name of target variable.}
#' \item{size[\code{integer(1)}]}{Number of cases.}
#' \item{n.feat [\code{integer}]}{Number of covariates, named vector with entries: \dQuote{numerics}, \dQuote{factors}.}
#' \item{class.levels [\code{character}]}{All possible classes. Character vector. NA if not classification.}
#' \item{has.missing [\code{logical(1)}]}{Are missing values present?}
#' \item{has.blocking [\code{logical(1)}]}{Is blocking available in task for observations?}
#' \item{positive [\code{character(1)}]}{Positive class label for binary classification, NA else.} 
#' \item{negative [\code{character(1)}]}{Negative class label for binary classification, NA else.} 
#' }
#' @name TaskDesc
#' @rdname TaskDesc
NULL

makeTaskDesc = function(data, target, type, id, has.blocking, positive) {
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
  td$has.missing = any(sapply(data, function(x) any(is.na(x))))
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