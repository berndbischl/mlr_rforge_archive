#' @include Parameter.r
roxygen()

#' Description class for a hyperparameter.
#'  
#' Getter.\cr
#' 
#' \describe{
#'  \item{has.default [boolean]}{Was a default value provided?}
#'  \item{default [any]}{Default value. Error if none was provided.}
#'  \item{when [string]}{Specifies when a cetrain hyperparameter is used. Possible entries are 'train', 'predict' or 'both'.}
#'  \item{requires [list]}{Requirements for a parameter to be effective.}
#' }
#' @exportClass Parameter
#' @title Description class for a hyperparameter. 
setClass(
  "Parameter.learner",
  contains = c("Parameter"),
  representation = representation(
    has.default = "logical",
    default = "ANY",
    when = "character",
    flags = "list",
    requires = "expression"	
  )	
)


#' Constructor.
setMethod(f = "initialize",
  signature = signature("Parameter.learner"),
  def = function(.Object, id, type, constraints, has.default, default, when, flags, requires) {
    if (missing(has.default))
      return(make.empty(.Object))
    .Object@has.default = has.default
    .Object@default = default
    .Object@when = when
    .Object@flags = flags
    .Object@requires = requires
    callNextMethod(.Object, id, type, constraints)
})



#' @rdname Parameter.learner-class
setMethod(
  f = "[",
  signature = signature("Parameter.learner"),
  def = function(x,i,j,...,drop) {
    if (i == "pass.default") {
      passd = x@flags$pass.default
      return(!is.null(passd) && passd)
    }
    callNextMethod()
  }
)
