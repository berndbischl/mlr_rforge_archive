#' @include object.r
roxygen()

#' Description object for task, encapsulates basic statistics without having to store the complete data set.
#' 
#' @slot type Type of task, either \dQuote{classif} for classification or \dQuote{regr} for regression.
#' @slot id Id string of task.
#' @slot target Name of target variable.
#' @slot size Number of cases.
#' @slot n.feat Number of covariates, named vector with entries: \dQuote{numerics}, \dQuote{integers}, \dQuote{factors}, \dQuote{characters}, \dQuote{logicals}.
#' @slot class.dist Class distribution. Named vector. NA if not classification.
#' @slot has.missing Are missing values present?
#' @slot has.inf Are infinite numerical values present?
#' @slot has.weights Are weights available in task for covariates?
#' @slot has.blocking Is blocking available in task for observations?
#' @slot positive Positive class label for binary classification, NA else. 
#' @slot negative Negative class label for binary classification, NA else. 
#'
#' @exportClass TaskDesc
#' @seealso \code{\linkS4class{LearnTask}}
#' @title Description object for task. 

setClass(
		"TaskDesc",
		contains = c("object"),
		representation = representation(
				type = "character",
        id = "character",
        target = "character",
        size = "integer",
        n.feat = "integer",
        class.dist = "integer",
        has.missing = "logical",
        has.inf = "logical",
        has.weights = "logical",
        has.blocking = "logical",
        positive = "character", 
        negative = "character" 
    )
)

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("TaskDesc"),
  def = function(.Object, data, target, type, id, has.weights, has.blocking, positive) {
    .Object@type = type
    .Object@id = id
    i = which(colnames(data) %in% c(target))
    .Object@target = target 
    .Object@size = nrow(data)
    y = data[, target]
    .Object@n.feat = c(
      numerics = sum(sapply(data, is.double)) - is.double(y), 
      integers  = sum(sapply(data, is.integer)) - is.integer(y),
      factors = sum(sapply(data, is.factor)) - is.factor(y),
      characters = sum(sapply(data, is.character)) - is.character(y),
      logicals = sum(sapply(data, is.logical)) - is.logical(y)
    )
    .Object@has.missing = any(is.na(data))
    .Object@has.inf = any(is.infinite(data))
    if(is.factor(y))
      .Object@class.dist = {tab=table(y);cl=as.integer(tab); names(cl)=names(tab);cl}
    else
      .Object@class.dist = as.integer(NA)
    
    .Object@has.weights = has.weights
    .Object@has.blocking = has.blocking
    .Object@positive = positive
    if(type == "classif" && length(class.dist) == 2) 
      .Object@negative = setdiff(names(.Object@class.dist), .Object@positive) 
    else 
      .Object@negative = as.character(NA)
    return(.Object)
  }
)
