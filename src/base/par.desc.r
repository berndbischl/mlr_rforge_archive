#' Description class for an optimization parameter.
#' Construct them with \code{numeric.parameter}, \code{integer.parameter}, \code{logical.parameter}, \code{discrete.parameter}. 
#'  
#' Getter.\cr
#' 
#' \describe{
#'  \item{name [string]}{Name of parameter.}
#'  \item{type [string]}{Type of parameter: numeric, integer, discrete, logical, unknown.}
#'  \item{lower [single numeric | NULL]}{Lower bound for numeric/integer parameter. \code{NULL} otherwise.}
#'  \item{upper [single numeric | NULL]}{Upper bound for numeric/integer parameter. \code{NULL} otherwise.}
#'  \item{vals [named list]}{Allowed values for discrete parameter. \code{NULL} otherwise.}
#' }
#' @exportClass par.desc
#' @seealso \code{\link{is.feasible}}, \code{\link{random.val}}, \code{\link{par.desc.learner}}, \code{\link{bounds}} 
#' @title Description class for a an optimization parameter.



setClass(
	"par.desc",
  contains = c("object"),
	representation = representation(
		name = "character",
    type = "character",
    constraints = "list"
	)	
)


#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("par.desc"),
		def = function(.Object, name, type, constraints) {
      if (missing(name))
        return(make.empty(.Object))
      .Object@name = name
      .Object@type = type
      .Object@constraints = constraints
      .Object
		}
)


#' @rdname par.desc-class
setMethod(
  f = "[",
  signature = signature("par.desc"),
  def = function(x,i,j,...,drop) {
    type = x@type
    if (i == "vals") {
      if (type != "discrete")
        return(NULL)
      else
        return(x@constraints$vals)
    }
    if (i == "lower") {
      if (type != "numeric" && type != "integer")
        return(NULL)
      else
        return(x@constraints$lower)
    }
    if (i == "upper") {
      if (type != "numeric" && type != "integer")
        return(NULL)
      else
        return(x@constraints$upper)
    }
    callNextMethod()
  }
)

