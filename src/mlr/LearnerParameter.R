#' @include Parameter.R
roxygen()

#' Description class for a hyperparameter.
#' 
#' @slot has.default Was a default value provided?
#' @slot pass.default Should the default value be always passed down to the underlying function? 
#' @slot default Default value for this parameter. 
#' @slot when Specifies when parameter is used. Possible entries are \dQuote{train}, \dQuote{predict} or \dQuote{both}.
#' @slot requires R expression over the other parameters to define requirements which make this parameter effective.
#' 
#' @exportClass LearnerParameter
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

