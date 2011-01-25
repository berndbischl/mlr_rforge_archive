#todo: switch on init conversion later, switch off now for tests

#' Numerical variable for optimization.
#' @param lower [single numeric] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [single numeric] \cr
#'   Upper bound. Default is \code{Inf}.
#' @return  \code{\linkS4class{par.desc}}
numeric.parameter = function(id, lower=-Inf, upper=Inf) {
  if (is.integer(lower))
    lower = as.numeric(lower)
  if (is.integer(upper))
    upper = as.numeric(upper)
  check.arg(lower, "numeric", 1)
  check.arg(upper, "numeric", 1)
  if (upper < lower)
    stop("No possible value!")
  constraints = list(lower=as.numeric(lower), upper=as.numeric(upper))
  new("par.desc", id, "numeric", constraints)
} 



#' Integer variable for optimization.
#' @param lower [single numeric] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [single numeric] \cr
#'   Upper bound. Default is \code{Inf}.
#' @return  \code{\linkS4class{par.desc}}
integer.parameter = function(id, lower=-.Machine$integer.max, upper=.Machine$integer.max, default) {
  if (is.numeric(lower) && length(lower)==1 && is.finite(lower) && lower==as.integer(lower))
    lower = as.integer(lower)
  if (is.numeric(upper) && length(upper)==1 && is.finite(upper) && upper==as.integer(upper))
    upper = as.integer(upper)
  check.arg(lower, "integer", 1)
  check.arg(upper, "integer", 1)
  if (upper < lower)
    stop("No possible value!")
  constraints = list(lower=as.integer(lower), upper=as.integer(upper))
  new("par.desc", id, "integer", constraints)
} 


#' Boolean variable for optimization.
#' @return  \code{\linkS4class{par.desc}}
logical.parameter = function(id) {
  new("par.desc", id, "logical", list())
} 

#' Discrete variable for optimization.
#' @param vals [named list] \cr
#'   Possible values.
#' @return  \code\linkS4class{par.desc}}
discrete.parameter = function(id, vals) {
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
  new("par.desc", id, "discrete", constraints)
} 

#' Untyped variable for optimization.
#' @return \code{\linkS4class{par.desc}}
untyped.parameter = function(id) {
  new("par.desc", id, "untyped", list())
} 

