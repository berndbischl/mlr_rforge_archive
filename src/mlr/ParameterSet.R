#' @include Parameter.R
roxygen()
#' @include Parameter_feasible.R
roxygen()

#' A bunch of parameters. Is initially created, then mainly queried for information but not changed. 
#'  
#' @exportClass ParameterSet
#' @title Optimization path

setClass(
  "ParameterSet",
  representation = representation(
    pars = "list"
  )
)


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("ParameterSet"),
  def = function(.Object, pars) {
    if(any(sapply(pars, function(x) !is(x, "Parameter"))))
      stop("All parameters must be of class 'Parameter'!")
    ns = sapply(pars, function(x) x@id)
    if (any(duplicated(ns)))
      stop("All parameters must have unique names!")
    names(pars) = ns
    .Object@pars = pars
    return(.Object)
  }
)

#'  Convert to list.
#' @rdname ParameterSet-class 
#' @export
setMethod(
  f = "as.list",
  signature = signature("ParameterSet"),
  def = function(x, row.names = NULL, optional = FALSE,...) {
    z = x@pars
    names(z) = sapply(z, function(y) y["par.name"])
    z
  }
)


#' Construct a ParameterSet from a bunch of Parameter objects.
#' 
#' @param ... [Some \code{\linkS4class{Parameter}} objects] \cr
#' @return [{\linkS4class{ParameterSet}}]
#' @rdname makeParameterSet
#' @export 
makeParameterSet = function(...) {
  args = list(...)
  checkListElementClass(args, "Parameter")
  new("ParameterSet", pars=args)
}



setMethod("show", "ParameterSet", function(object) {
  sapply(object@pars, print)
})
  

#' @rdname isFeasible
setMethod(
  f = "isFeasible",
  signature = signature(par="ParameterSet", x="list"),
  def = function(par, x) {
    all(mapply(isFeasible, par@pars, x))
  }
)



#' Get lower bounds.
#' @param x [\code{\linkS4class{Parameter}} | \code{\linkS4class{ParameterSet}}] \cr
#'   A single parameter or a set of parameters.
#' @return Numeric vector of lower bounds. If \code{x} is a ParameterSet the vector is named. 
#' @rdname lower
#' @exportMethod lower
setGeneric(name = "lower", def = function(x, ...) standardGeneric("lower"))
#' @rdname lower
setMethod(
  f = "lower",
  signature = signature(x="Parameter"), 
  def = function(x) 
    if(!x@type %in% c("integer", "numeric", "numericvector", "integervector")) 
      stop("Only available for numeric, integer, numericvector and integervector parameter!") 
    else 
      x@constraints$lower
)
#' @rdname lower
setMethod(
  f = "lower",
  signature = signature(x="ParameterSet"), 
  def = function(x, select=c("integer", "numeric", "numericvector", "integervector")) { 
    v = Filter(function(y) y@type %in% select, x@pars)
    z = lapply(v, function(y) y@constraints$lower)
    ns = Reduce(c, Map(function(nn, ll) rep(nn, length(ll)), names(z), z))
    z = Reduce(c, z)
    names(z) = ns
    return(z)
  }
)

#' Get upper bounds.
#' @param x [\code{\linkS4class{Parameter}} | \code{\linkS4class{ParameterSet}}] \cr
#'   A single parameter or a set of parameters.
#' @return Numeric vector of upper bounds. If \code{x} is a ParameterSet the vector is named. 
#' @rdname upper
#' @exportMethod upper
setGeneric(name = "upper", def = function(x, ...) standardGeneric("upper"))
#' @rdname upper
setMethod(
  f = "upper",
  signature = signature(x="Parameter"), 
  def = function(x) 
    if(!x@type %in% c("integer", "numeric", "numericvector", "integervector")) 
      stop("Only available for numeric, integer, numericvector and integervector parameter!") 
    else 
      x@constraints$upper
)
#' @rdname upper
setMethod(
  f = "upper",
  signature = signature(x="ParameterSet"), 
  def = function(x, select=c("integer", "numeric", "numericvector", "integervector")) { 
    v = Filter(function(y) y@type %in% select, x@pars)
    z = lapply(v, function(y) y@constraints$upper)
    ns = Reduce(c, Map(function(nn, ll) rep(nn, length(ll)), names(z), z))
    z = Reduce(c, z)
    names(z) = ns
    return(z)
  }
)

#' Get discrete values.
#' @param x [\code{\linkS4class{Parameter}} | \code{\linkS4class{ParameterSet}}] \cr
#'   A single discrete parameter or a set of parameters.
#' @param only.names [logical(1)] \cr
#'   If \code{TRUE} forces the return only value names as strings, otherwise the real values are returned.
#'   Default is \code{FALSE}.
#' @return For a single parameter: Its list of values or a character vector if \code{only.names} is \code{TRUE}.
#'  For a Parameterset: A named list of lists of values or a list of character vectors (names of values). 
#' @rdname values
#' @exportMethod values
setGeneric(name = "values", def = function(x, ...) standardGeneric("values"))
#' @rdname values
setMethod(
  f = "values",
  signature = signature(x="Parameter"), 
  def = function(x, only.names=FALSE) 
    if(!x@type %in% c("discrete")) 
      stop("Only available for discrete parameter!") 
    else { 
      if (only.names)
        names(x@constraints$vals)
      else
        x@constraints$vals
    }
)
#' @rdname values
setMethod(
  f = "values",
  signature = signature(x="ParameterSet"), 
  def = function(x, select=c("discrete"), only.names=FALSE) { 
    v = Filter(function(y) y@type %in% select, x@pars)
    if (only.names)
      z = lapply(v, function(y) names(y@constraints$vals))
    else
      z = lapply(v, function(y) y@constraints$vals)
    names(z) = sapply(v, function(y) y@id)
    return(z)
  }
)

valToString = function(par, val) {
  if (is(par, "ParameterSet")) {
    if (isProperlyNamed(val)) {
      ns = names(val)
      val = Map(valToString, par@pars[ns], val)
    } else {  
      ns = names(par@pars)
      val = Map(valToString, par@pars, val)
    }
    paste(ns, val, sep="=", collapse=",")
  } else {
    type = par@type
    if (type == "numeric")
      as.character(round(val, 3))  
    else if (type == "numericvector")
      paste(as.character(round(val, 3)), collapse=",")  
    else if (type == "integer" || type == "logical")
      as.character(val)  
    else if (type == "discrete" || type == "ordered") {
      vals = par@constraints$vals
      if (is.character(val) && length(val) == 1 && val %in% names(vals)) {
        val
      } else {
        i = which(sapply(vals, function(v) almost.equal(val, v)))
        names(vals)[i]
      }
    } else if (type == "function"){
      "<function>" 
    } else if (type == "untyped"){
      class(val)
    }
  }
}


c.ParameterSet = function(..., recursive=FALSE) {
  pss = list(...)
  pars = Reduce(c, lapply(pss, function(ps) ps@pars))
  new("ParameterSet", pars=pars)  
}




