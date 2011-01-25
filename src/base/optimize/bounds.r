#' @include par.desc.r
roxygen()
#' @include par.desc.feasible.r
roxygen()

#' Bounds for a bunch of parameters. Are initially created, then mainly queried for information but not changed. 
#'  
#' @exportClass bounds
#' @title Optimazation path

setClass(
  "bounds",
  contains = c("object"),
  representation = representation(
    pars = "list"
  )
)


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("bounds"),
  def = function(.Object, pars) {
    if(any(sapply(pars, function(x) !is(x, "par.desc"))))
      stop("All bounds parameters must be of class 'par.desc'!")
    ns = sapply(pars, function(x) x@id)
    if (any(duplicated(ns)))
      stop("All bounds parameters must have unique names!")
    names(pars) = ns
    .Object@pars = pars
    return(.Object)
  }
)

#'  Convert to list.
#' @rdname bounds-class 
#' @export
setMethod(
  f = "as.list",
  signature = signature("bounds"),
  def = function(x, row.names = NULL, optional = FALSE,...) {
    z = x@pars
    names(z) = sapply(z, function(y) y["par.name"])
    z
  }
)


#' Construct a bounds object.
#' 
#' @param pars [list of \code{\linkS4class{learn.task}}] \cr
#' 			Type of the learning algorithm, either "classif" or "regr" or task to solve
#' @param doubles [boolean] \cr
#' 			Supports real-valued inputs? Pass only when x is a string.
#' @param factors [boolean] \cr
#' 			Supports factor inputs? Pass only when x is a string.
#' @param characters [boolean] \cr
#' 			Supports character inputs? Pass only when x is a string.
#' @param missings [boolean] \cr
#' 			Supports missing values? Pass only when x is a string.
#' @param multiclass [boolean] \cr
#' 			Supports multiclass problems? Pass only when x is a string.
#' @param weights [boolean] \cr
#' 			Supports case weights? Pass only when x is a string.
#' @param probs [boolean] \cr
#' 			Can predict probabilities?
#' @param decision [boolean] \cr
#' 			Supports decision values?
#' @param costs [boolean] \cr
#' 			Supports non-standard misclassification costs?
#' 
#' @rdname get.learners
#' @export 
#' 
#' @title Find matching learning algorithms.


make.bounds = function(...) {
  args = list(...)
  if (length(args) == 0)
    stop("Bounds are empty!")
  new("bounds", pars=args)
}


#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("bounds"),
  def = function(x) {
    paste(sapply(x@pars, to.string), collapse="\n")
  }
)


setMethod(
  f = "is.feasible",
  signature = signature(x="list", bounds="bounds"),
  def = function(x, bounds) {
    all(mapply(is.feasible, x, bounds@pars))
  }
)


setGeneric(name = "lower", def = function(x) standardGeneric("lower"))

setMethod(
  f = "lower",
  signature = signature(x="par.desc"), 
  def = function(x) 
    if(!x@type %in% c("integer", "numeric")) 
      stop("Only available for numeric or integer parameter!") 
    else 
      x@constraints$lower
)

setMethod(
  f = "lower",
  signature = signature(x="bounds"), 
  def = function(x) { 
    v = Filter(function(y) y@type %in% c("integer", "numeric"), x@pars)
    z = sapply(v, function(y) y@constraints$lower)
    names(z) = sapply(v, function(y) y@id)
    return(z)
  }
)

setGeneric(name = "upper", def = function(x) standardGeneric("upper"))

setMethod(
  f = "upper",
  signature = signature(x="par.desc"), 
  def = function(x) 
    if(!x@type %in% c("integer", "numeric")) 
      stop("Only available for numeric or integer parameter!") 
    else 
      x@constraints$upper
)

setMethod(
  f = "upper",
  signature = signature(x="bounds"), 
  def = function(x) { 
    v = Filter(function(y) y@type %in% c("integer", "numeric"), x@pars)
    z = sapply(v, function(y) y@constraints$upper)
    names(z) = sapply(v, function(y) y@id)
    return(z)
  }
)



