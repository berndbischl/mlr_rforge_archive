##' Check if a parameter setting satisfies the constraints of the
##' parameter description.
##' 
##' @param x [single value] \cr
##'   Value to check.  
##' @param bounds [par.desc] \cr
##'   Variable description.
##' @return If \code{x} is a single value, a logical, else a vector of
##' logicals as long as the vector \code{x}. Note that for discrete
##' parameters a heuristic is used to figure out if \code{x} is a list
##' of parameters or in fact a discrete parameter of type list.
##' 
##' @rdname is.feasible
##' @exportMethod is.feasible
##' 
##' @title Check if parameter setting is valid.
setGeneric(
  name = "is.feasible",
  def = function(x, bounds) {
    standardGeneric("is.feasible")      
  }
)

#' @rdname is.feasible
setMethod(
  f = "is.feasible",
  signature = signature(x="ANY", bounds="par.desc"),
  def = function(x, bounds) {
    type = bounds["type"]
    if (type == "numeric")
      is.numeric(x) & x >= bounds["lower"] & y <= bounds["upper"] 
    if (type == "integer")
      (is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))) & x >= bounds["lower"] && y <= bounds["upper"]
    else if (type == "discrete")
      if(is.null(x)) any(is.null(bound["vals"])) else x %in% bounds["vals"]
    else if (type == "logical")
      is.logical(x) & !is.na(x)
    else 
      stop("Unknown type!")
  }
)
