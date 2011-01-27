#' @include par.desc.r
roxygen()

#' Check if a parameter setting satisfies the constraints of the
#' parameter description.
#' 
#' @param x [single value] \cr
#'   Value to check.  
#' @param par.set [par.desc] \cr
#'   Variable description.
#' @return If \code{x} is a single value, a logical, else a vector of
#' logicals as long as the vector \code{x}. Note that for discrete
#' parameters a heuristic is used to figure out if \code{x} is a list
#' of parameters or in fact a discrete parameter of type list.
#' 
#' @rdname is.feasible
#' @exportMethod is.feasible
#' 
#' @title Check if parameter setting is valid.
setGeneric(
  name = "is.feasible",
  def = function(x, par.set) {
    if (length(x) == 0)
      return(FALSE)
    standardGeneric("is.feasible")      
  }
)

#' @rdname is.feasible
setMethod(
  f = "is.feasible",
  signature = signature(x="ANY", par.set="par.desc"),
  def = function(x, par.set) {
    type = par.set["type"]
    if (type == "numeric")
      is.numeric(x) & x >= lower(par.set) & x <= upper(par.set)
    else if (type == "integer")
      (is.integer(x) | (is.numeric(x) & (x == as.integer(x)))) & x >= par.set@constraints$lower & x <= par.set@constraints$upper
    else if (type == "discrete") {
      g = function(y) 
        !is.na(Position(function(v) identical(y, v), par.set@constraints$vals)) ||
        !is.na(Position(function(v) identical(y, v), names(par.set@constraints$vals))) 
      if (length(x) == 1) return(g(x)) else return(sapply(x, g))
      #if(is.null(x)) any(is.null(bound["vals"])) else x %in% par.set["vals"]
    } else if (type == "logical") {
      is.logical(x) & !is.na(x)
    } else if (type == "function") {
      if (is.function(x))
        TRUE
      else if (is.list(x))
        sapply(x, is.function)
      else
        FALSE
    } else if (type == "untyped")
      if (is.vector(x))
        rep(TRUE, length(x))
      else
        TRUE
    else 
      stop("Unknown type!")
  }
)
