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
#' @exportClass par.desc
#' @title Description class for a hyperparameter. 

setClass(
  "par.desc.learner",
  contains = c("par.desc"),
  representation = representation(
    has.default = "logical",
    default = "ANY",
    when = "character",
    flags = "list",
    requires = "expression"	
  )	
)


#' @rdname par.desc.learner-class

setMethod(
  f = "[",
  signature = signature("par.desc.learner"),
  def = function(x,i,j,...,drop) {
    if (i == "pass.default") {
      passd = x@flags$pass.default
      return(!is.null(passd) && passd)
    }
    callNextMethod()
  }
)
