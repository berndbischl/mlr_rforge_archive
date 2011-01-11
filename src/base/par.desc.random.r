#' Draw n random values from a variable description.
#' 
#' @param n [integer] \cr
#'   Value to check.  
#' @param var [par.desc] \cr
#'   Variable description.
#' 
#' @rdname random.val
#' @export 
#' @return [atomic value].


setGeneric(
  name = "random.val",
  def = function(n, var) {
    if (is.numeric(n))
      n = as.integer(n)
    standardGeneric("random.val")      
  }
)

setMethod(
  f = "random.val",
  signature = signature(n="integer", var="par.desc.disc"),
  def = function(n, var) 
    as.factor(sample(names(pd["vals"]), n, replace=TRUE))
)

setMethod(
  f = "random.val",
  signature = signature(n="integer", var="par.desc.log"),
  def = function(n, var) 
    sample(c(TRUE, FALSE), n, replace=TRUE)
)
