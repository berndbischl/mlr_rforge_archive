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
  signature = signature(x="ANY", bounds="par.desc"),
  def = function(x, bounds) {
    type = bounds["type"]
    if (type == "numeric")
      is.numeric(x) & x >= bounds["lower"] & y <= bounds["upper"] 
    if (type == "integer")
      (is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))) & x >= bounds["lower"] && y <= bounds["upper"]
    else if (type == "discrete")
      x %in% bounds["vals"]
    else if (type == "logical")
      is.logical(x) & x %in% c(TRUE, FALSE)
    else 
      stop("Unknown type!")
  }
)
