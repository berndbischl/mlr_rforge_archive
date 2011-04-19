#' Get property of learner object.
#' #' @slot id Id of learner, used in printing and list indexing.
#' @slot type Can be 'classif' or 'regr'.
#' @slot feat Which types of features can be used? Has 2 named elements 'numerics' and 'factors'.
#' @slot weights Length 1. Can case weights be used? 
#' @slot missings Length 1. Can missing values be handled? na.omit does not count. 
#' @slot classes Which types of classes can be handled? 
#'   Has 3 named elements 'oneclass', 'twoclass' and 'multiclass'.
#'   For regression this is \code{c(oneclass=FALSE, twoclass=FALSE, multiclass=FALSE)}.  
#' @slot predict Which types of predictions (other than 'response') can be made? 
#'   Has 2 named elements 'prob' and 'decision'.  
#'   For regression this is \code{c(prob=FALSE, twoclass=FALSE, decision=FALSE)}.  
#' @slot costs Length 1. Can misclassification costs be handled? 
#'   For regression this is \code{FALSE}. 
#'  
#' 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner object.   
#' @param prop [\code{character(1)}] \cr
#'   Name of property.
#'        
#' @return Property values.
#' @exportMethod getProperty
#' @title Get property of learner object.
#' @seealso \code{\link{setProperty}}, \code{\link{setProperties}} 
#' @rdname getProperty 

setGeneric(
  name = "getProperty",
  def = function(learner, prop) {
    standardGeneric("getProperty")
  }
)

#' @rdname getProperty 
setMethod(
  f = "getProperty", signature = signature(learner="Learner", prop="character"),
  def = function(learner, prop) {
    if (!prop %in% 
      c("type", "numerics", "factors", "missings", "weights", "oneclass", "twoclass", 
        "multiclass", "decision", "prob", "costs"))
      stop("Requested unknown property ", prop, " for learner ", learner@id, "!")      
    learner@properties[[prop]]
  } 
)


#' Set property of learner object.  
#' 
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'   Learner object.
#' @param prop [\code{character(1)}] \cr
#'   Name of property.
#'        
#' @return \code{\linkS4class{Learner}}.
#' @exportMethod setProperty
#' @title Set property of learner object.  
#' @seealso \code{\link{getProperty}},  \code{\link{setProperties}}
#' @rdname setProperty 

setGeneric(
  name = "setProperty",
  def = function(learner, prop, val) {
    standardGeneric("setProperty")
  }
)

#' @rdname setProperty 
setMethod(
  f = "setProperty", signature = signature(learner="Learner", prop="character"),
  def = function(learner, prop, val) {
    if (!prop %in% 
      c("type", "numerics", "factors", "missings", "weights", "oneclass", "twoclass", 
        "multiclass", "decision", "prob", "costs"))
      stop("Tried to set unknown property ", prop, " for learner ", learner@id, "!")      
    learner@properties[[prop]] = val
    return(learner)
  } 
)


#' Set properties of learner object.  
#' 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner object.   
#' @param ...  \cr
#'   Names and values as named arguments
#'        
#' @return \code{\linkS4class{Learner}}.
#' @exportMethod setProperties
#' @title Set properties of learner object.  
#' @seealso \code{\link{getProperty}}, \code{\link{setProperty}} 
#' @rdname setProperties

setGeneric(
  name = "setProperties",
  def = function(learner, ...) {
    standardGeneric("setProperties")
  }
)

#' @rdname setProperties
setMethod(
  f = "setProperties", signature = signature(learner="Learner"),
  def = function(learner, ...) {
    args = list(...)
    ns = names(args)
    for (i in seq_along(args))
      learner = setProperty(learner, ns[i], args[[i]])
    return(learner)
  } 
)




