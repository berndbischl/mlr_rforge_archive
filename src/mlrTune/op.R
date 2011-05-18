#' Optimazation path. Optimizers log their evaluated points
#' iteratively into this object.
#' 
#' Can be converted to a list or data.frame.
#' 
#' @exportClass OptPath
#' @title Optimazation path
setClass(
  "OptPath",
  representation = representation(
    x.names = "character",
    y.names = "character",
    minimize = "logical",
    env = "environment"
  )
)

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("OptPath"),
  def = function(.Object, x.names, y.names, minimize) {
    if (missing(x.names))
      return(mlr:::make.empty(.Object))
    if (length(intersect(x.names, y.names)) > 0)
      stop("'x.names' and 'y.names' must not contain common elements!")
    if (length(minimize) != length(y.names))
      stop("'y.names' and 'minimize' must be of the same length!")
    if (is.character(names(minimize)) && !setequal(names(minimize), y.names))
      stop("Given names for 'minimize' must be the same as 'y.names'!")
    if (is.null(names(minimize)))
      names(minimize) = y.names
    
    if (any(c("dob", "eol") %in% (union(x.names, y.names))))
      stop("'dob' and 'eol' are not allowed in 'x.names' or 'y.names'!")
    .Object@x.names = x.names
    .Object@y.names = y.names
    .Object@minimize = minimize
    .Object@env = new.env()
    return(.Object)
  }
)



#' Add a new element to the optimiztion path.
#' 
#' @param op [\code{\linkS4class{OptPath}}] \cr 
#'   Optimization path.  
#' @param x [\code{list}]\cr 
#'   List of parameter settings for a point in input space. Must be in same order as \code{x.names}.  
#' @param y [numeric] \cr 
#'   Vector of fitness values.  Must be in same order as \code{y.names}.
#' @param dob [integer(1)] \cr 
#'   Date of birth of the new parameters. Default is length of path + 1.  
#' @param eol [integer(1)] \cr 
#'   End of life of point. Defaults to unknown (NA). 
#' @return NULL. This function is called for its side effects, namely
#'   adding \code{x} to the optimization path.
#' @export 

setGeneric(
  name = "addPathElement",
  def = function(op, x, y, dob, eol) {
    if (missing(dob))        
      dob = getLength(op)+1L
    if (missing(eol))        
      eol = as.integer(NA)
    mlr:::check.arg(dob, "integer", 1)
    mlr:::check.arg(eol, "integer", 1)
    stopifnot(is.na(eol) || eol >= dob)
    standardGeneric("addPathElement")
  }
)


setGeneric(
  name = "getLength",
  def = function(op) {
    standardGeneric("getLength")
  }
)

#' Set the end of life of a parameter vector.
#'
#' @param op [\code{\linkS4class{OptPath}}] \cr 
#'   Optimization path.  
#' @param x [\code{integer(1)}]\cr 
#'   Index of element in path.  
#' @param eol [integer(1)] \cr 
#'   End of life of point. 
#' @return NULL, this function is called for its side effect, namely
#'   modifing the optimization path.
setEoL = function(op, index, eol) {
  op@env$eol[index] = as.integer(eol)
  NULL
} 



#' @export
setMethod(f = "show", signature = signature("OptPath"), def = function(object) {
    cat("Opt. path of length: ", getLength(object), "\n")
})












