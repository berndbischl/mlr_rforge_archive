#' @include Parameter.R
roxygen()

#' Check if a parameter setting satisfies the constraints of the
#' parameter description.
#' 
#' @param x [single value] \cr
#'   Value to check.  
#' @param par [Parameter] \cr
#'   Variable description.
#' @return logical(1) 
#' 
#' @rdname isFeasible
#' @exportMethod isFeasible
#' 
#' @title Check if parameter setting is valid.
setGeneric(
  name = "isFeasible",
  def = function(par, x) {
    if (length(x) == 0)
      return(FALSE)
    standardGeneric("isFeasible")      
  }
)

#' @rdname isFeasible
setMethod(
  f = "isFeasible",
  signature = signature(par="Parameter", x="ANY"),
  def = function(par, x) {
    type = par["type"]
    if (type == "numeric")
      is.numeric(x) && length(x) == 1 && x >= lower(par) & x <= upper(par)
    else if (type == "integer")
      is.numeric(x) && length(x) == 1 && (x == as.integer(x)) && x >= lower(par) & x <= upper(par)
    else if (type == "numericvector")
      is.numeric(x) && length(x) == length(lower(par)) && all(x >= lower(par)) & all(x <= upper(par))
    else if (type == "integervector")
      is.numeric(x) && length(x) == length(lower(par)) && all(x == as.integer(x)) && all(x >= lower(par)) & all(x <= upper(par))
    else if (type == "discrete") {
        !is.na(Position(function(v) isTRUE(all.equal(x, v)), par@constraints$vals)) ||
        !is.na(Position(function(v) identical(x, v), names(par@constraints$vals))) 
    } else if (type == "logical") {
      is.logical(x) && length(x) == 1 && !is.na(x)
    } else if (type == "function") {
      is.function(x)
    } else if (type == "untyped")
      TRUE
    else 
      stop("Unknown type!")
  }
)
