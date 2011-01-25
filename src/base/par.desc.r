#' Description class for an optimization parameter.
#' 
#' @exportClass par.desc
#' @seealso \code{\link{is.feasible}}, \code{\link{random.val}}, \code{\link{par.desc.learner}}, \code{\link{make.bounds}} 
#' @title Description class for an optimization parameter.

setClass("par.desc",
  contains = c("object"),
  representation = representation(
    id = "character",
    type = "character",
    constraints = "list",
    scale = "function" 
))

#' Constructor.
setMethod(f = "initialize",
  signature = signature("par.desc"),
  def = function(.Object, id, type, constraints, scale=identity) {
    if (missing(id))
      return(make.empty(.Object))
    .Object@id = id
    .Object@type = type
    .Object@constraints = constraints
    .Object@scale = scale
    .Object
})



#' @rdname to.string
setMethod(
  f = "to.string",
  signature = signature("par.desc"),
  def = function(x) {
    type = x["type"]
    if (type == "numeric")
      paste("Numeric parameter '", x@id, "'. Bounds: ", x@constraints$lower, ",", x@constraints$upper, sep="")  
    else if (type == "integer")
      paste("Integer parameter '", x@id, "'. Bounds: ", x@constraints$lower, ",", x@constraints$upper, sep="")  
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
