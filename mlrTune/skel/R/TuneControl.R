#' Abstract base class for control objects for tuning.
#'  
#' Cannot be instantiated. 
#' 
#' \describe{
#'   \item{TuneControlGrid}{Grid search. All kinds of parameter types can be handled, but you have discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}}.}
#'   \item{TuneControlOptim}{\code{\link[stats]{optim}}. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#'   \item{TuneControlCMAES}{CMA Evolution Strategy. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#' }
#' 
#' Subclasses: \code{\linkS4class{TuneControlGrid}}, \code{\linkS4class{TuneControlOptim}}, \code{\linkS4class{TuneControlCMAES}}
NULL

makeTuneControl = function(path, same.resampling.instance, start, ...) {
  if (!is.null(names(start)))
    stop("'start' has to be a unamed list, but in the correct order!")
	x = makeOptControl(path=path, same.resampling.instance=same.resampling.instance, ...)
  x$start = start       
  return(x)
}