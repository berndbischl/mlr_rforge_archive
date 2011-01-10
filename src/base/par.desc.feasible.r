#' Is a value compliable with the bounds / type of a variable.
#' 
#' @param x [single value] \cr
#'   Value to check.  
#' @param bounds [par.desc] \cr
#'   Variable description.
#' 
#' @rdname is.feasible
#' @exportMethod is.feasible
#' @return [boolean].
#' 
#' @title Find matching learning algorithms.

setGeneric(
  name = "is.feasible",
  def = function(x, bounds) {
    standardGeneric("is.feasible")      
  }
)

#' @rdname is.feasible
setMethod(
  f = "is.feasible",
  signature = signature(x="numeric", bounds="par.desc.double"),
  def = function(x, bounds) {
    length(x)==1 && x >= bounds["lower"] && y <= bounds["upper"] && (bounds["data.type"] == "numeric" || is.integer(x) || x == as.integer(x))
  }
)

#' @rdname is.feasible
setMethod(
  f = "is.feasible",
  signature = signature(x="numeric", bounds="par.desc.disc"),
  def = function(x, bounds) {
    length(x)==1 && any(sapply(bounds["vals"], function(a) identical(x, a)))
  }
)

#' @rdname is.feasible
setMethod(
  f = "is.feasible",
  signature = signature(x="logical", bounds="par.desc.log"),
  def = function(x, bounds) {
    length(x)==1 
  }
)

