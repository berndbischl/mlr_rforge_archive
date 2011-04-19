#' @include Parameter.R
roxygen()

#' Description class for a hyperparameter.
#'  
#' Getter.\cr
#' 
#' \describe{
#'  \item{has.default [boolean]}{Was a default value provided?}
#'  \item{default [any]}{Default value. Error if none was provided.}
#'  \item{when [\code{character(1)}]}{Specifies when a cetrain hyperparameter is used. Possible entries are 'train', 'predict' or 'both'.}
#'  \item{requires [list]}{Requirements for a parameter to be effective.}
#' }
#' @exportClass Parameter
#' @title Description class for a hyperparameter. 
setClass(
  "LearnerParameter",
  contains = c("Parameter"),
  representation = representation(
    has.default = "logical",
    pass.default = "logical",
    default = "ANY",
    when = "character",
    requires = "expression"	
  )	
)


#' Constructor.
setMethod(f = "initialize",
  signature = signature("LearnerParameter"),
  def = function(.Object, id, type, constraints, has.default, default, pass.default, when, requires) {
    if (missing(has.default))
      return(make.empty(.Object))
    .Object@has.default = has.default
    .Object@default = default
    .Object@pass.default = pass.default
    .Object@when = when
    .Object@requires = requires
    callNextMethod(.Object, id, type, constraints)
})

