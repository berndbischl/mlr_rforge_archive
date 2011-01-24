#' Description class for an optimization parameter.
#' 
#' Construct them with \code{link{numeric.parameter}}, \code{\link{integer.parameter}, 
#' \code{\link{logical.parameter}} or \code{\link{discrete.parameter}}. 
#' 
#' 
#' @slot id [character(1)] Name of parameter.  
#' @slot type [character(1)] Type of parameter: 'numeric', 'integer', 'discrete', 'logical'.  
#' @slot constraints [list] List of constraints, for numeric/integer this contains this contains numeric(1) 
#'   entries 'lower' and 'upper', for 'discrete' it contains the named list 'vals' of values.  
#' @slot scale [function(x)] Optional function to rescale a parameter. Defaults to 'identity'.   
#' 
#' @exportClass par.desc
#'
#' @seealso \code{\link{is.feasible}}, \code{\link{random.val}},  \code{\link{par.desc.learner}},  \code{\link{make.bounds}} 
#' 
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
          def = function(.Object, id, type, constraints, scale) {
            if (missing(id))
              return(make.empty(.Object))
            .Object@id = id
            .Object@type = type
            .Object@constraints = constraints
            ns = names(formals(scale))
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
      paste("Numeric parameter '", x["name"], "'. Bounds: ", x["lower"], ",", x["upper"], sep="")  
    else if (type == "integer")
      paste("Integer parameter '", x["name"], "'. Bounds: ", x["lower"], ",", x["upper"], sep="")  
    else if (type == "discrete") {
      paste("Discrete parameter '", x["name"], "'. Values: ", paste(names(x["vals"]), collapse=","), sep="") 
    } else if (type == "logical") {
      paste("Logical parameter '", x["name"], "'.", sep="") 
    } else if (type == "untyped"){
      paste("Untyped parameter '", x["name"], "'.", sep="") 
    } else 
      stop("Unknown type!")
  }
)
