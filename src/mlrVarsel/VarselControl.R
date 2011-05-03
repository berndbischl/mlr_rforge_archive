#' @importClassesFrom mlrTune OptControl
roxygen()

#' Abstract base class for control objects for variable selection. 
#' Cannot be instantiated.
#' 
#' \describe{
#'   \item{VarselControlExhaustive}{Exhaustive search. All feature sets (up to a certain size) are searched.}
#'   \item{VarselControlRandom}{Random search. Features vectors are randomly drawn.}
#'   \item{VarselControlSequential}{Deterministic forward or backward search.}
#' }
#' 
#' Subclasses: \code{\link{VarselControlExhaustive}}, \code{\link{VarselControlRandom}}, \code{\link{VarselControlSequential}} 
#' 
#' @exportClass VarselControl
#' @title Base class for control objects for variable selection.

setClass(
		"VarselControl",
		contains = c("OptControl"),
		representation = representation(
				compare = "character",
				max.vars = "integer", 
				maxit = "integer"
		)
)

#' Constructor.
setMethod(
		f = "initialize",
		signature = signature("VarselControl"),
		def = function(.Object, path, same.resampling.instance, maxit, max.vars) {
      if (missing(path))
        return(mlr:::make.empty(.Object))
			.Object@compare = "diff" 			
			.Object@max.vars = as.integer(max.vars) 			
			.Object@maxit = as.integer(maxit) 		
			.Object = callNextMethod(.Object, path, same.resampling.instance)
			return(.Object)
		}
)

setMethod(f = "show",  signature = signature("VarselControl"), def = function(object) {
  cat(
    "Control object for varsel of class: ", class(x), "\n",
    "Save path: ", x@path, "\n",
    "Same resampling instance: ", x@same.resampling.instance, "\n",
    "Max. vars: ", x@max.vars, "\n",
    "Max. iter: ", x@maxit,  
    sep=""
  )
})



