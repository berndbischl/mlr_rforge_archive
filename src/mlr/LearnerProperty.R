#' Get property of learner object. Currently available are:
#' \describe{
#'  \item{type [\code{character(1)}]}{Can be \dQuote{classif} or \dQuote{regr}.}
#'  \item{numerics [\code{logical(1)}]}{Can numeric features be handled?}
#'  \item{factors [\code{logical(1)}]}{Can factor features values be handled?}
#'  \item{missings [\code{logical(1)}]}{Can missing values be handled?}
#'  \item{weights [\code{logical(1)}]}{Can case weights be handled?}
#'  \item{oneclass [\code{logical(1)}]}{Can one-class problems be handled?}
#'  \item{twoclass [\code{logical(1)}]}{Can two-class problems be handled?}
#'  \item{multiclass [\code{logical(1)}]}{Can multi-class problems be handled?}
#'  \item{prob [\code{logical(1)}]}{Can probabilities be predicted?}
#' }
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
        "multiclass", "prob"))
      stop("Requested unknown property ", prop, " for learner ", learner@id, "!")      
    learner@properties[[prop]]
  } 
)


#' Set property of learner object. For available properties see \code{\link{getProperty}}.
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
        "multiclass", "prob"))
      stop("Tried to set unknown property ", prop, " for learner ", learner@id, "!")      
    learner@properties[[prop]] = val
    return(learner)
  } 
)


#' Set properties of learner object.  
#' 
#' @param learner [\code{\linkS4class{Learner}}]\cr 
#'   Learner object.   
#' @param ... [any] \cr
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




