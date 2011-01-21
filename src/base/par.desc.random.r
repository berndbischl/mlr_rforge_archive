#' Draw n random values from a variable description.
#' 
#' @param n [integer] \cr
#'   Value to check.  
#' @param x [par.desc] \cr
#'   Variable description.
#' 
#' @rdname random.val
#' @export 
#' @return [atomic value].


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
  signature = signature(n="integer", x="par.desc"),
  def = function(n, x) {
    type = x["type"]
    if (type == "numeric")
      runif(n, min=x["lower"], max=x["upper"])
    else if (type == "integer")
      as.integer(round(runif(n, min=x["lower"], max=x["upper"])))
    else if (type == "discrete") {
      as.factor(sample(names(x["vals"]), n, replace=TRUE))
    } else if (type == "logical") {
      sample(c(TRUE, FALSE), n, replace=TRUE)
    } else if (type == "untyped")
      stop("Cannot generate random val for untyped variable!")
    else 
      stop("Unknown type!")
  }
)

