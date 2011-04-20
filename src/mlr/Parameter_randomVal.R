##' Draw \code{n} random values uniformly from a parameter description.
##' 
##' @param par [\code{\linkS4class{Parameter}}] \cr
##'   Parameter description.
##' @param n [\code{integer}] \cr
##'   Values to sample.  
##' 
##' @rdname randomVal
##' @exportMethod randomVal
##' @return Vector of values. Will be 
##'   \code{numeric(n)} for a numeric parameter, 
##'   \code{integer(n)} for a numeric parameter,
##'   \code{character(n)} for a numeric parameter,
##'   \code{logical(n)} for a logical parameter,
##'   \code{list(n) of } for a logical parameter,
#
#
#setGeneric(
#  name = "randomVal",
#  def = function(par, n) {
#    if (is.numeric(n))
#      n = as.integer(n)
#    standardGeneric("randomVal")      
#  }
#)
#
#
##' @rdname randomVal
#setMethod(
#  f = "randomVal",
#  signature = signature(par="Parameter", n="integer"),
#  def = function(par, n) {
#    type = par["type"]
#    if (type == "numeric")
#      runif(n, min=par@constraints$lower, max=par@constraints$upper)
#    else if (type == "numericvector")
#      runif(n, min=par@constraints$lower, max=par@constraints$upper)
#    else if (type == "integer")
#      as.integer(round(runif(n, min=par@constraints$lower, max=par@constraints$upper)))
#    else if (type == "discrete") {
#      #todo: should this return names or values?
#      sample(names(par@constraints$vals), n, replace=TRUE)
#    } else if (type == "logical") {
#      sample(c(TRUE, FALSE), n, replace=TRUE)
#    } else if (type == "untyped")
#      stop("Cannot generate random val for untyped variable!")
#    else 
#      stop("Unknown type!")
#  }
#)

