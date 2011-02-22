#todo: switch on init conversion later, switch off now for tests
#todo: ordered

#' Numerical variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param trafo [function(x)] \cr
#'   Function to transform parameter. Is applied to the parameter before it is passed to its corresponding fitness function. Default is \code{\link{identity}}.   
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeNumericParameter = function(id, lower=-Inf, upper=Inf, trafo=identity) {
  if (is.integer(lower))
    lower = as.numeric(lower)
  if (is.integer(upper))
    upper = as.numeric(upper)
  check.arg(lower, "numeric", 1)
  check.arg(upper, "numeric", 1)
  if (upper < lower)
    stop("No possible value!")
  constraints = list(lower=lower, upper=upper)
  new("Parameter", id, "numeric", constraints, trafo)
} 

#' Numeric vector variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @param dim [integer(1)] \cr
#'   Length of vector.
#' @param lower [numeric(dim)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(dim)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param trafo [function(x)] \cr
#'   Function to transform parameter. Is applied to the parameter before it is passed to its corresponding fitness function. Default is \code{\link{identity}}.   
#' @return \code{\linkS4class{Parameter}}
#' @export 
makeNumericVectorParameter = function(id, dim, lower=-Inf, upper=Inf, trafo=identity) {
  if (is.integer(lower))
    lower = as.numeric(lower)
  if (is.integer(upper))
    upper = as.numeric(upper)
  if (is.numeric(lower) && length(lower) == 1)
    lower = rep(lower, dim)
  if (is.numeric(upper) && length(upper) == 1)
    upper = rep(upper, dim)
  check.arg(lower, "numeric", dim)
  check.arg(upper, "numeric", dim)
  if (any(upper < lower))
    stop("No possible value!")
  constraints = list(lower=lower, upper=upper)
  new("Parameter", id, "numericvector", constraints, trafo)
} 

#' Integer variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @param lower [numeric(1)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(1)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param trafo [function(x)] \cr
#'   Function to transform parameter. Is applied to the parameter before it is passed to its corresponding fitness function. Default is \code{\link{identity}}.   
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeIntegerParameter = function(id, lower=-.Machine$integer.max, upper=.Machine$integer.max, trafo=identity) {
  if (is.numeric(lower) && length(lower)==1 && is.finite(lower) && lower==as.integer(lower))
    lower = as.integer(lower)
  if (is.numeric(upper) && length(upper)==1 && is.finite(upper) && upper==as.integer(upper))
    upper = as.integer(upper)
  check.arg(lower, "integer", 1)
  check.arg(upper, "integer", 1)
  if (upper < lower)
    stop("No possible value!")
  constraints = list(lower=lower, upper=upper)
  new("Parameter", id, "integer", constraints, trafo)
} 

#' Numeric vector variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @param dim [integer(1)] \cr
#'   Length of vector.
#' @param lower [numeric(dim)] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [numeric(dim)] \cr
#'   Upper bound. Default is \code{Inf}.
#' @param trafo [function(x)] \cr
#'   Function to transform parameter. Is applied to the parameter before it is passed to its corresponding fitness function. Default is \code{\link{identity}}.   
#' @return \code{\linkS4class{Parameter}}
#' @export 
makeIntegerVectorParameter = function(id, dim, lower=-.Machine$integer.max, upper=.Machine$integer.max, trafo=identity) {
  if (is.integer(lower) && all(is.finite(lower)) && all(lower==as.integer(lower)))
    lower = as.integer(lower)
  if (is.integer(upper) && all(is.finite(upper)) && all(upper==as.integer(upper)))
    upper = as.integer(upper)
  if (is.integer(lower) && length(lower) == 1)
    lower = rep(lower, dim)
  if (is.integer(upper) && length(upper) == 1)
    upper = rep(upper, dim)
  check.arg(lower, "integer", dim)
  check.arg(upper, "integer", dim)
  if (any(upper < lower))
    stop("No possible value!")
  constraints = list(lower=lower, upper=upper)
  new("Parameter", id, "integervector", constraints, trafo)
} 



#' Boolean variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeLogicalParameter = function(id) {
  new("Parameter", id, "logical", list())
} 

#' Discrete variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @param vals [list | vector] \cr
#'   Possible values.
#' @return  \code{\linkS4class{Parameter}}
#' @export 
makeDiscreteParameter = function(id, vals) {
  if (is.vector(vals))
    vals = as.list(vals)
  check.arg(vals, "list")
  if (length(vals)==0)
    stop("No possible value!")
  n = length(vals)
  # if names missing, set all to ""
  if (is.null(names(vals)))
    names(vals) = rep("", n)
  # guess missing names
  ns = names(vals)
  for (i in 1:n) {
    v = vals[[i]]
    if(is.na(ns[i]) || ns[i] == "") {
      if (is.character(v) || is.numeric(v))
        names(vals)[i] = as.character(v)
    }
  }  
  if(!all.els.named(vals)) {
    stop("Not all values for par. ", id,  " were named and names could not be guessed!")
  }
  if(any(duplicated(names(vals))))
    stop("Not all names for par. ", id,  " are unique!")
  constraints = list(vals=vals)
  new("Parameter", id, "discrete", constraints)
} 


#' Function variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @return \code{\linkS4class{Parameter}}
#' @export 
makeFunctionParameter = function(id) {
  new("Parameter", id, "function", list())
} 


#' Untyped variable for optimization.
#' @param id [character(1)]
#'   Name of parameter.
#' @return \code{\linkS4class{Parameter}}
#' @export 
makeUntypedParameter = function(id) {
  new("Parameter", id, "untyped", list())
} 




