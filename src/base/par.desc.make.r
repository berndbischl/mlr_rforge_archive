#' Numerical variable for optimization.
#' @param lower [single numeric] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [single numeric] \cr
#'   Upper bound. Default is \code{Inf}.
#' @return  \code\linkS4class{par.desc}}
#' @rdname par.desc-class
numeric.parameter = function(name, lower=-Inf, upper=Inf) {
  #if (is.integer(lower))
  #  lower = as.numeric(lower)
  #if (is.integer(upper))
  #  upper = as.numeric(upper)
  check.arg(lower, "numeric", 1)
  check.arg(upper, "numeric", 1)
  if (upper < lower)
    stop("No possible value!")
  constraints = list(lower=as.numeric(lower), upper=as.numeric(upper))
  new("par.desc", name, "numeric", constraints)
} 



#' Integer variable for optimization.
#' @param lower [single numeric] \cr
#'   Lower bound. Default is \code{-Inf}.
#' @param upper [single numeric] \cr
#'   Upper bound. Default is \code{Inf}.
#' @return  \code\linkS4class{par.desc}}
#' @rdname par.desc-class
integer.parameter = function(name, lower=-.Machine$integer.max, upper=.Machine$integer.max, default) {
  #if (is.numeric(lower) && lower == as.integer(lower))
  #  lower = as.integer(lower)
  #if (is.numeric(upper) && upper == as.integer(upper))
  #  upper = as.integer(upper)
  check.arg(lower, "integer", 1)
  check.arg(upper, "integer", 1)
  if (upper < lower)
    stop("No possible value!")
  constraints = list(lower=as.integer(lower), upper=as.integer(upper))
  new("par.desc", name, "integer", constraints)
} 


#' Boolean variable for optimization.
#' @return  \code\linkS4class{par.desc}}
#' @rdname par.desc-class
logical.parameter = function(name) {
  new("par.desc", name, "logical", list())
} 

#' Discrete variable for optimization.
#' @param vals [named list] \cr
#'   Possible values.
#' @return  \code\linkS4class{par.desc}}
#' @rdname par.desc-class
discrete.parameter = function(name, vals) {
  if (is.vector(vals))
    vals = as.list(vals)
  check.arg(vals, "list")
  if (is.vector(vals))
    vals = as.list(vals)
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
    stop("Not all values for par. ", name,  " were named and names could not be guessed!")
  }
  
  constraints = list(vals=vals)
  new("par.desc", name, "discrete", constraints)
} 

#' Untyped variable for optimization.
#' @return  \code\linkS4class{par.desc}}
#' @rdname par.desc-class
untyped.parameter = function(name) {
  new("par.desc", name, "untyped", list())
} 

