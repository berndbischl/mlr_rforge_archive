#' Create control structures for tuning.
#' 
#' \describe{
#'   \item{TuneControlGrid}{Grid search. All kinds of parameter types can be handled, but you have discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}}.}
#'   \item{TuneControlOptim}{\code{\link[stats]{optim}}. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#'   \item{TuneControlCMAES}{CMA Evolution Strategy. Can handle numeric and integer hyperparameters. For integers the internally proposed numeric values are rounded.}
#' }
#' 
#' #' Use optimizations algorithms from \code{\link{optim}}, e.g. Nelder-Mead, SANN, etc. 
#' Subclasses: \code{\linkS4class{TuneControlGrid}}, \code{\linkS4class{TuneControlOptim}}, \code{\linkS4class{TuneControlCMAES}}
#' Create control structure for tuning. 
#' 
#' @title Control for CMA-ES tuning. 
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [\code{numeric}]\cr
#'   Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[cmaes]{cma_es}}.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[stats]{optim}}.
#' @return [\code{\link{TuneControl}}]. The specific subclass is one of
#'   \code{\link{TuneControlGrid}}, \code{\link{TuneControlOptim}}, \code{\link{TuneControlCMAES}}.
#' @name TuneControl
#' @rdname TuneControl
NULL

makeTuneControl = function(same.resampling.instance, start, ..., cl) {
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  if (!is.null(names(start)))
    stop("'start' has to be a unamed list, but in the correct order!")
	x = makeOptControl(same.resampling.instance=same.resampling.instance, ...)
  x$start = start       
  class(x) = c(cl, "TuneControl", class(x))
  return(x)
}

print.OptControl = function(x, ...) {
  catf("Tune control: %s", class(x)[1])
  catf("  Same resampling instance: %s", x$same.resampling.instance)
  catf("  Start: %s", listToShortString(x$start))
  catf("  Further arguments: %s", listToShortString(x$extra.args))
}
