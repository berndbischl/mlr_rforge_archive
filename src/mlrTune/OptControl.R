#' @include aaa.R
roxygen()

#' Abstract base class for control objects for optimization. 
#' Cannot be instatiated. 
#' 
#' Getter.\cr
#' 
#' @exportClass OptControl
#' @seealso \code{\linkS4class{TuneControl}}, \code{\linkS4class[mlrVarsel]{VarselControl}} 
#' @title Base class for control objects for optimization.

setClass(
		"OptControl",
		representation = representation(
				path = "logical",
        same.resampling.instance = "logical",        
				extra.args = "list"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("OptControl"),
		def = function(.Object, path, same.resampling.instance, ...) {
      if (missing(path))
        return(mlr:::make.empty(.Object))
			.Object@path = path
      .Object@same.resampling.instance = same.resampling.instance
			.Object@extra.args = list(...)
			return(.Object)
		}
)

