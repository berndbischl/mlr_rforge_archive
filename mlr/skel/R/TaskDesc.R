#' Description object for task, encapsulates basic statistics without having to store the complete data set.
#' 
#' @slot type Type of task, either \dQuote{classif} for classification or \dQuote{regr} for regression.
#' @slot id Id string of task.
#' @slot target Name of target variable.
#' @slot size Number of cases.
#' @slot n.feat Number of covariates, named vector with entries: \dQuote{numerics}, \dQuote{integers}, \dQuote{factors}, \dQuote{characters}, \dQuote{logicals}.
#' @slot class.levels All possible classes. Character vector. NA if not classification.
#' @slot has.missing Are missing values present?
#' @slot has.inf Are infinite numerical values present?
#' @slot has.blocking Is blocking available in task for observations?
#' @slot positive Positive class label for binary classification, NA else. 
#' @slot negative Negative class label for binary classification, NA else. 
#'
#' @exportClass TaskDesc
#' @seealso \code{\linkS4class{LearnTask}}
#' @title Description object for task. 
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
    integers  = sum(sapply(data, is.integer)) - is.integer(y),
    factors = sum(sapply(data, is.factor)) - is.factor(y),
    characters = sum(sapply(data, is.character)) - is.character(y),
    logicals = sum(sapply(data, is.logical)) - is.logical(y)
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