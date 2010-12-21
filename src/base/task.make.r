#' @include task.classif.r
roxygen()
#' @include task.regr.r
roxygen()
#' @include prepare.df.r
roxygen()

#' Defines a learning task for a given data set. 
#' The type (classification or regression) is automatically inferred from the target variable.
#' It might perform some data conversions in the data.frame, like converting integer input features to numerics, 
#' but will generally warn about this. If you want to change default preprocessing behaviour, look at
#' \code{\link{prepare.control}}, construct the control object yourself and pass it into the \code{control} argument 
#' of \code{make.task}.
#' Whether a classification or regression task is created depends on the data type of the target variable. 
#' A factor, logical or character vector produces a classification task (and the vector is converted to a factor), 
#' numerics produce regression tasks. Integer target variables have to be changed manually. 
#' 
#' @param id [string]\cr 
#'   Id string for object. Used to select the object from a named list, etc. Default is the name of the passed R variable.  
#' @param data [data.frame] \cr 	
#'   A data frame containing the input and target variables for modeling.
#' @param target [string] \cr
#'   Name of the target variable.
#' @param exclude [character]
#'   Names of inputs, which should be generally disregarded, e.g. IDs, etc. Default is zero-length vector. 
#' @param weights [numeric] \cr 	
#'   An optional vector of case weights to be used in the fitting process (if the learner cannot handle weights, they are ignored). Default is not to use weights.
#' @param blocking [factor] \cr 	
#'   An optional factor of the same length as the number of observations. Observations with the same blocking level "belong together". Specifically, they are either put all in the training or the test set during a resampling iteration.
#' @param control [\code{\linkS4class{prepare.control}}] \cr 	
#'	 Optional control object used for preparing the data.frame. For defaults look at \code{\link{prepare.control}}.
#' @param costs [matrix] \cr 	
#'   An optional matrix of misclassification costs to be used in the fitting process. 
#'   If the used classifier can handle cost matrices it is passed down to its train function, otherwise it is ignored.
#'   Rows indicate true classes, columns predicted classes.
#' 	 Don't pass this in case of regression.	
#' @param positive [string] \cr 	
#'   Positive class for binary classification. Default is the first factor level of the target attribute. 
#' 	 Don't pass this in case of regression.	
#' 
#' 
#' @return \code{\linkS4class{learn.task}}.
#' 
#' @export
#' @rdname make.task
#' 
#' @title Construct learning task.

setGeneric(
  name = "make.task",
  def = function(id, data, target, exclude, weights, blocking, control, costs, positive) {
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
    if (missing(costs)) 
      costs = matrix(0,0,0)
    check.arg(costs, "matrix")
    if (missing(positive))
      positive = as.character(NA)
    check.arg(positive, "character", 1)
    if (missing(control))
      control = prepare.control()
    standardGeneric("make.task")
  }
)


#' @export
setMethod(
  f = "make.task",
  
  signature = signature(
    id="character", 
    data="data.frame", 
    target="character", 
    exclude="character", 
    weights="numeric", 
    blocking="factor",
    control="prepare.control",
    costs="matrix",
    positive="character"
  ),
  
  def = function(id, data, target, exclude, weights, blocking, control, costs, positive) {
    
    if(length(weights) > 0 && length(weights) != nrow(data))
      stop("Weights have to be of the same length as number of rows in data! Or pass none at all.")
    if(length(blocking) > 0 && length(blocking) != nrow(data))
      stop("Blockings have to be of the same length as number of rows in data! Or pass none at all.")
    
    cns = colnames(data)
    x = duplicated(cns)
    if(any(x))
      stop("Duplicated column names in data.frame are not allowed: ", paste(cns[x], collapse=","))
    if (!(target %in% cns)) {
      stop(paste("Column names of data.frame don't contain target var: ", target))
    }
    
    # todo: rpart does not like (), bug there?
    forbidden  = c("[", "]", "(", ")", ",", " ")
    forbidden2 = c("[", "]", "(", ")", ",", "<WHITESPACE>")
    #forbidden = c("[", "]")
    i = sapply(forbidden, function(x) length(grep(x, cns, fixed=TRUE)) > 0)
    if (any(i))
      stop(paste("Column names should not contain: ", paste(forbidden2, collapse=" ")))
    if (any(is.na(data[, target]))) {
      stop("Target values contain missings!")
    }
    if (any(is.infinite(data[, target]))) {
      stop("Target values contain infinite values!")
    }
    
    if (!all(exclude %in% cns))
      stop("Trying to exclude non-existing variables: ", setdiff(exclude, cns))
    if (target %in% exclude)
      stop("Trying to exclude target variable!")
    
    if(is.factor(data[,target]) || is.character(data[,target]) || is.logical(data[,target]))
      type = "classif"
    else if(is.numeric(data[,target]) && !is.integer(data[,target]))
      type = "regr"
    else 
      stop("Cannot infer the type of task from the target data type. Please transform it!")
    
    
    data = prep.data(type=="classif", data, target, exclude, control)			
    
    if (type == "classif") {
      new("classif.task", id=id, target=target, data=data, exclude=exclude, weights=weights, blocking=blocking, control=control, costs=costs, positive=positive)
    } else {
      if(!is.na(positive))
        stop("You cannot define a positive class for regression!")
      if (!(all(dim(costs) == 0)))
        stop("You cannot define a cost matrix for regression!")
      new("regr.task", id=id, target=target, data=data, exclude=exclude, weights=weights, blocking=blocking, control=control)
    }
  }
)




