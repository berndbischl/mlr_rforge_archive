#' Draw \code{n} random values uniformly from a parameter description.
#' 
#' @param n [integer] \cr
#'   Values to sample.  
#' @param x [\code{\linkS4class{Parameter}}] \cr
#'   Parameter description.
#' 
#' @rdname random.val
#' @export 
#' @return Vector of values. Will be 
#'   \code{numeric(n)} for a numeric parameter, 
#'   \code{integer(n)} for a numeric parameter,
#'   \code{character(n)} for a numeric parameter,
#'   \code{logical(n)} for a logical parameter,
#'   \code{list(n) of } for a logical parameter,


setGeneric(
  name = "random.val",
  def = function(n, x) {
    if (is.numeric(n))
      n = as.integer(n)
    standardGeneric("random.val")      
  }
)


#' @rdname random.val
setMethod(
  f = "random.val",
  signature = signature(n="integer", x="Parameter"),
  def = function(n, x) {
    type = x["type"]
    if (type == "numeric")
      runif(n, min=x@constraints$lower, max=x@constraints$upper)
    else if (type == "integer")
      as.integer(round(runif(n, min=x@constraints$lower, max=x@constraints$upper)))
    else if (type == "discrete") {
      #todo: should this return names or values?
      sample(names(x@constraints$vals), n, replace=TRUE)
    } else if (type == "logical") {
      sample(c(TRUE, FALSE), n, replace=TRUE)
    } else if (type == "untyped")
      stop("Cannot generate random val for untyped variable!")
    else 
      stop("Unknown type!")
  }
)

