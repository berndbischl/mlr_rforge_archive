#' Description class for an optimization parameter.
#'
#' Subclasses: \code{\linkS4class{LearnerParameter}}
#' 
#' @slot id Name of parameter. 
#' @slot type Data type of parameter. Possible types are 'numeric', 'integer', 'ordered', 'discrete', 'function', 'untyped'.
#' @slot constraints Contains further information of simple contraints. 
#' For 'numeric' and 'integer' it contains box constraints 'lower' and 'upper', both \code{numeric(1)}. 
#' For 'discrete' and 'ordered' it contains possible values 'vals' as a list, for 'ordered' the order of the list reflects the implied ordering. 
#' @slot trafo When values for the parameter are generated in any way, this function will always be applied directly aftwerwards. Function must accept a parameter value as the first argument and return a transformed one.
#' 
#' @exportClass Parameter
#' @seealso \code{\link{makeParameterSet}} 
#' @title Description class for an optimization parameter.

setClass("Parameter",
  contains = c("object"),
  representation = representation(
    id = "character",
    type = "character",
    constraints = "list",
    trafo = "function" 
))

#' Constructor.
setMethod(f = "initialize",
  signature = signature("Parameter"),
  def = function(.Object, id, type, constraints, trafo=identity) {
    if (missing(id))
      return(make.empty(.Object))
    .Object@id = id
    .Object@type = type
    .Object@constraints = constraints
    .Object@trafo = trafo
    .Object
})



#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("Parameter"),
  def = function(x) {
    type = x["type"]
    if (type == "numeric")
      paste("Numeric parameter '", x@id, "'. Constraints: ", x@constraints$lower, ",", x@constraints$upper, sep="")  
    else if (type == "integer")
      paste("Integer parameter '", x@id, "'. Constraints: ", x@constraints$lower, ",", x@constraints$upper, sep="")  
    else if (type == "discrete") {
      paste("Discrete parameter '", x@id, "'. Values: ", paste(names(x@constraints$vals), collapse=","), sep="") 
    } else if (type == "logical") {
      paste("Logical parameter '", x@id, "'.", sep="") 
    } else if (type == "ordered") {
      paste("Ordered parameter '", x@id, "'.", sep="") 
    } else if (type == "function"){
      paste("Function parameter '", x@id, "'.", sep="") 
    } else if (type == "untyped"){
      paste("Untyped parameter '", x@id, "'.", sep="") 
    } else 
      stop("Unknown type!")
  }
)

valToString = function(par, val) {
  type = par["type"]
  if (type == "numeric")
    formatC(val, digits=3)  
  else if (type == "integer" || type == "logical")
    as.character(val)  
  else if (type == "discrete" || type == "ordered") {
    vals = par@constraints$vals
    if (is.character(val) && length(val) == 1 && val %in% names(vals)) {
      val
    } else {
        i = which(sapply(vals, function(v) almost.equal(val, v)))
        names(vals)[i]
    }
  } else if (type == "function"){
    "<function>" 
  } else if (type == "untyped"){
    class(val)
  }
}

