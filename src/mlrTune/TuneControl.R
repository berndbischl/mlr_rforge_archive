#' @include OptControl.R
roxygen()

#' Abstract base class for control objects for tuning. 
#' Cannot be instantiated. 
#' 
#' \describe{
#'   \item{TuneControlGrid}{Grid search. All kinds of parameter types can be handled, but you have discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}}.}
#'   \item{TuneControlOptim}{\code{\link[stats]{optim}}. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#'   \item{TuneControlCMAES}{CMA Evolution Strategy. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#' }
#' 
#' Subclasses: \code{\linkS4class{TuneControlGrid}}, \code{\linkS4class{TuneControlOptim}}, \code{\linkS4class{TuneControlCMAES}}
#' 
#' @exportClass TuneControl
#' @title Base class for control objects for tuning.

setClass(
		"TuneControl",
		contains = c("OptControl"),
		representation = representation(
				start = "list"
		)
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("TuneControl"),
		def = function(.Object, path, same.resampling.instance, start, ...) {
      if (missing(path))
        return(mlr:::make.empty(.Object))
      if (!is.null(names(start)))
        stop("'start' has to be a unamed list, but in the correct order!")        
			.Object@start = start 			
			.Object = callNextMethod(.Object=.Object, path=path, same.resampling.instance=same.resampling.instance, ...)
			return(.Object)
		}
)
