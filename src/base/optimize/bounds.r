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
  contains = c("object"),
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


#' Construct a ParameterSet object.
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


makeParameterSet = function(...) {
  args = list(...)
  new("ParameterSet", pars=args)
}


#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("ParameterSet"),
  def = function(x) {
    paste(sapply(x@pars, to.string), collapse="\n")
  }
)


setMethod(
  f = "is.feasible",
  signature = signature(x="list", par.set="ParameterSet"),
  def = function(x, par.set) {
    all(mapply(is.feasible, x, par.set@pars))
  }
)


#' @exportMethod lower
setGeneric(name = "lower", def = function(x, ...) standardGeneric("lower"))

#' @export 
setMethod(
  f = "lower",
  signature = signature(x="Parameter"), 
  def = function(x) 
    if(!x@type %in% c("integer", "numeric")) 
      stop("Only available for numeric or integer parameter!") 
    else 
      x@constraints$lower
)

#' @export 
setMethod(
  f = "lower",
  signature = signature(x="ParameterSet"), 
  def = function(x, select=c("integer", "numeric")) { 
    v = Filter(function(y) y@type %in% select, x@pars)
    z = sapply(v, function(y) y@constraints$lower)
    names(z) = sapply(v, function(y) y@id)
    return(z)
  }
)

#' @exportMethod upper
setGeneric(name = "upper", def = function(x, ...) standardGeneric("upper"))

#' @export 
setMethod(
  f = "upper",
  signature = signature(x="Parameter"), 
  def = function(x) 
    if(!x@type %in% c("integer", "numeric")) 
      stop("Only available for numeric or integer parameter!") 
    else 
      x@constraints$upper
)

#' @export 
setMethod(
  f = "upper",
  signature = signature(x="ParameterSet"), 
  def = function(x, select=c("integer", "numeric")) { 
    v = Filter(function(y) y@type %in% select, x@pars)
    z = sapply(v, function(y) y@constraints$upper)
    names(z) = sapply(v, function(y) y@id)
    return(z)
  }
)

#' @exportMethod values
setGeneric(name = "values", def = function(x, ...) standardGeneric("values"))

#' @export 
setMethod(
  f = "values",
  signature = signature(x="Parameter"), 
  def = function(x, only.names=FALSE) 
    if(!x@type %in% c("discrete", "logical")) 
      stop("Only available for numeric or integer parameter!") 
    else { 
      if (only.names)
        names(x@constraints$vals)
      else
        x@constraints$vals
    }
)

#' @export 
setMethod(
  f = "values",
  signature = signature(x="ParameterSet"), 
  def = function(x, select=c("discrete", "logical"), only.names=FALSE) { 
    v = Filter(function(y) y@type %in% select, x@pars)
    if (only.names)
      z = lapply(v, function(y) names(y@constraints$vals))
    else
      z = lapply(v, function(y) y@constraints$vals)
    names(z) = sapply(v, function(y) y@id)
    return(z)
  }
)


#' @exportMethod trafoVal
setGeneric(name = "trafoVal", def = function(par, val) standardGeneric("trafoVal"))

#' @export 
setMethod(
  f = "trafoVal",
  signature = signature(par="Parameter", val="ANY"), 
  def = function(par, val) {
    par@trafo(val)
  }
)

#' @export 
setMethod(
  f = "trafoVal",
  signature = signature(par="ParameterSet", val="list"), 
  def = function(par, val) {
    Map(trafoVal, par@pars, val)
  }
)

#' @export 
setMethod(
  f = "valToString",
  signature = signature(par="ParameterSet", val="list"), 
  def = function(par, val) {
    if (all.els.named(val)) {
      ns = names(val)
      val = Map(valToString, par@pars[ns], val)
    } else {  
      ns = names(par@pars)
      val = Map(valToString, par@pars, val)
    }
    paste(ns, val, sep="=", collapse=",")
  }
)

# todo: check that learner and normal parameters are not mixed
c.ParameterSet = function(..., recursive=FALSE) {
  pss = list(...)
  pars = Reduce(c, lapply(pss, function(ps) ps@pars))
  new("ParameterSet", pars=pars)  
}




