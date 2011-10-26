#' Description class for an optimization parameter.
#'
#' Subclasses: \code{\linkS4class{LearnerParameter}}
#' 
#' @slot id Name of parameter. 
#' @slot type Data type of parameter. Possible types are 'numeric', 'numericvector', 'integer', 'ordered', 'discrete', 'function', 'untyped'.
#' @slot constraints Contains further information of simple contraints. 
#' For 'numeric' and 'integer' it contains box constraints 'lower' and 'upper', both \code{numeric(1)}. 
#' For 'discrete' and 'ordered' it contains possible values 'vals' as a list, for 'ordered' the order of the list reflects the implied ordering. 
#' For 'numericvector' it contains box constraints 'lower' and 'upper', both \code{numeric(dimension of vector)}. 
#' @slot trafo When values for the parameter are generated in any way, this function will always be applied directly aftwerwards. Function must accept a parameter value as the first argument and return a transformed one.
#' 
#' @exportClass Parameter
#' @seealso \code{\link{makeParameterSet}} 
#' @title Description class for an optimization parameter.

setClass("Parameter",
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



setMethod("show", "Parameter", function(object) {
  type = object@type
  ut = !identical(object@trafo, identity)
  if (type == "numeric")
    cat("Numeric parameter '", object@id, "'. Constraints: ", object@constraints$lower, " to ", object@constraints$upper, 
      ". Custom trafo: ", ut, "\n", sep="")  
  else if (type == "integer")
    cat("Integer parameter '", object@id, "'. Constraints: ", object@constraints$lower, " to ", object@constraints$upper, 
      ". Custom trafo: ", ut, "\n", sep="")  
  else if (type == "numericvector")
    cat("Numeric vector parameter '", object@id, "'. Constraints: ", 
      paste(object@constraints$lower, collapse=","), " to ", paste(object@constraints$upper, collapse=","), 
      ". Custom trafo: ", ut, "\n", sep="")  
  else if (type == "integervector")
    cat("Numeric vector parameter '", object@id, "'. Constraints: ", 
      paste(object@constraints$lower, collapse=","), " to ", paste(object@constraints$upper, collapse=","), 
      ". Custom trafo: ", ut, "\n", sep="")  
  else if (type == "discrete") {
    cat("Discrete parameter '", object@id, "'. Values: ", paste(names(object@constraints$vals), collapse=","), "\n", sep="") 
  } else if (type == "logical") {
    cat("Logical parameter '", object@id, "'.\n", sep="") 
  } else if (type == "ordered") {
    cat("Ordered parameter '", object@id, "'.\n", sep="") 
  } else if (type == "function"){
    cat("Function parameter '", object@id, "'.\n", sep="") 
  } else if (type == "untyped"){
    cat("Untyped parameter '", object@id, "'.\n", sep="") 
  } else 
    stop("Unknown type!")
})



