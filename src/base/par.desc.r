#' Description class for an optimization parameter.
#'
#' @slot id Name of parameter. 
#' @slot type Data type of parameter. Possible types are 'numeric', 'integer', 'ordered', 'discrete', 'function', 'untyped'.
#' @slot constraints Contains further information of simple contraints. 
#' For 'numeric' and 'integer' in contains box constraints 'lower' and 'upper', both \code{numeric(1)}. 
#' For 'discrete' and 'ordered' in contains possible values 'vals' as a list. 
#' @slot type Data type of parameter. Possible types are 'numeric', 'integer', 'ordered', 'discrete', 'function', 'untyped'.
#' @slot trafo When values for the parameter are generated in any way, this function will always be applied directly aftwerwards. Function must accept a parameter value as the first argument and return a transformed one.
#' 
#' @exportClass par.desc
#' @seealso \code{\link{par.desc.learner}}, \code{\link{makeParameterSet}} 
#' @title Description class for an optimization parameter.

setClass("par.desc",
  contains = c("object"),
  representation = representation(
    id = "character",
    type = "character",
    constraints = "list",
    trafo = "function" 
))

#' Constructor.
setMethod(f = "initialize",
  signature = signature("par.desc"),
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
  signature = signature("par.desc"),
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
    } else if (type == "untyped"){
      paste("Untyped parameter '", x@id, "'.", sep="") 
    } else 
      stop("Unknown type!")
  }
)
