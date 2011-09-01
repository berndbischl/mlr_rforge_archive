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
    par.set = "ParameterSet",
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
  def = function(.Object, par.set, y.names, minimize) {
    if (missing(par.set))
      return(mlr:::make.empty(.Object))
    x.names = getRepeatedParameterIDs(par.set, with.nr=TRUE)
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
    .Object@par.set = par.set
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
#' @param x.trafo [\code{list}]\cr 
#'   If parameters were transformed, the transformed values. Same structure as \code{x}.
#'   If missing, no transformation was done, and it is set to \code{x}.   
#' @param y [numeric] \cr 
#'   Vector of fitness values.  Must be in same order as \code{y.names}.
#' @param dob [integer(1)] \cr 
#'   Date of birth of the new parameters. Default is length of path + 1.  
#' @param eol [integer(1)] \cr 
#'   End of life of point. Defaults to unknown (NA). 
#' @return NULL. This function is called for its side effects, namely
#'   adding \code{x} to the optimization path.
#' @exportMethod addPathElement 

setGeneric(
  name = "addPathElement",
  def = function(op, x, x.trafo, y, dob, eol) {
    if (missing(x.trafo))        
      x.trafo = x
    if (missing(dob))        
      dob = getLength(op)+1L
    if (missing(eol))        
      eol = as.integer(NA)
    mlr:::check.arg(dob, "integer", 1)
    mlr:::check.arg(eol, "integer", 1)
    stopifnot(is.na(eol) || eol >= dob)
    if(!isFeasible(op@par.set, x))
      stop("Trying to add infeasible x values to opt path!")
    standardGeneric("addPathElement")
  }
)

#' @exportMethod getLength
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


getYVector = function(op, y.name) {
  if (is(op, "OptPathDF"))
    return(op@env$path[, y.name])
  #y = sapply(op@env$path, function(e) e$y[y.name])
}  

#' Get index of best element from optimiztion path.
#'
#' @param op [\code{\linkS4class{OptPath}}]\cr
#'   Optimization path.
#' @param y.name [\code{character(1)}] 
#'   Name of target value to decide which element is best.
#'   Default is \code{y.names[1]}.  
#' @param dob [\code{integer}]\cr
#'   Possible dates of birth to select best element from. Defaults to all. 
#' @param eol [\code{integer}]\cr
#'   Possible end of life to select best element from. Defaults to all. 
#' @param ties [\code{character(1)}]\cr
#'   How should ties be broken when more than one optimal element is found?
#'   \dQuote{all}: return all indices, \dQuote{first} return first optimal element in path,
#'   \dQuote{last} return last optimal element in path, \dQuote{random} return random optimal element in path.
#'   Default is \dQuote{all}.
#' @return [\code{integer}]
#'   Index or indices into path. See \code{ties}.
#' @export
getBestIndex = function(op, y.name=op@y.names[1], dob=op@env$dob, eol=op@env$eol, ties="all") {
  mlr:::check.arg(ties, "character", 1, c("all", "first", "last", "random"))
  life.inds = which(op@env$dob %in% dob & op@env$eol %in% eol)
  if (length(life.inds) == 0)
    stop("No element found which matches dob and eol restrictions!")
  y = getYVector(op, y.name)[life.inds]
  if (all(is.na(y))) {
    best.inds = life.inds  
  } else { 
    if (op@minimize[y.name])
      best.inds = which(min(y, na.rm=TRUE) == y)
    else 
      best.inds = which(max(y, na.rm=TRUE) == y)
    best.inds = life.inds[best.inds]
  }
  if (length(best.inds) > 1) {
    if (ties == "all")
      return(best.inds)
    else if (ties == "first")
      return(best.inds[1])
    else if (ties == "last")
      return(best.inds[length(best.inds)])
    else if (ties == "random")
      return(best.inds[sample(length(best.inds), 1)])
  } else {
    return(best.inds)
  }  
}

#' Get element from optimiztion path.
#'
#' @param op [\code{\linkS4class{OptPath}}]\cr
#'   Optimization path.
#' @param index [\code{integer(1)}] 
#'   Index of element.  
#' @return List with elements 'x' [list] and 'y' [named numeric]. 
#' @exportMethod getPathElement
setGeneric(
  name = "getPathElement",
  def = function(op, index) {
    if (is.numeric(index) && length(index) == 1 && index == as.integer(index))
      index = as.integer(index)
    mlr:::check.arg(index, "integer", 1)
    n = getLength(op)
    if (!(index >= 1 && index <= n))
      stop("Index must be between 1 and ", n, "!")
    standardGeneric("getPathElement")
  }
)








