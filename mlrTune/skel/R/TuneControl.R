#' Create control structures for tuning.
#' 
#' The following tuners are available:
#' \describe{
#'   \item{makeTuneControlGrid}{Grid search. All kinds of parameter types can be handled, but you have discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}} in the \code{par.set} passed to \code{\link{tune}}.}
#'   \item{makeTuneControlOptim}{Tuning with \code{\link[stats]{optim}}. Can handle numeric(vector) and integer(vector) hyperparameters. For integers the internally proposed numeric values are automatically rounded.}
#'   \item{makeTuneControlCMAES}{CMA Evolution Strategy. Can handle numeric(vector) and integer(vector) hyperparameters. For integers the internally proposed numeric values are automatically rounded.}
#' }
#' 
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is \code{TRUE}.
#' @param start [\code{numeric}]\cr
#'   Named list of initial parameter values.
#' @param ... [any]\cr
#'   Further control parameters passed to the \code{control} argument of \code{\link[stats]{optim}}
#'   and  to the \code{control} argument of \code{\link[cmaes]{cma_es}}. 
#' @return [\code{\link{TuneControl}}]. The specific subclass is one of
#'   \code{\link{TuneControlGrid}}, \code{\link{TuneControlGrid}}, \code{\link{TuneControlCMAES}}.
#' @name TuneControl
#' @rdname TuneControl
#' @aliases TuneControlGrid TuneControlGrid TuneControlCMAES
NULL

makeTuneControl = function(same.resampling.instance, start, ..., cl) {
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  checkArg(start, "list")
  if (!isProperlyNamed(start))
    stop("'start' must be a properly named list!")
	x = makeOptControl(same.resampling.instance=same.resampling.instance, ...)
  x$start = start       
  class(x) = c(cl, "TuneControl", class(x))
  return(x)
}

#S3method print TuneControl
print.OptControl = function(x, ...) {
  catf("Tune control: %s", class(x)[1])
  catf("  Same resampling instance: %s", x$same.resampling.instance)
  catf("  Start: %s", listToShortString(x$start))
  catf("  Further arguments: %s", listToShortString(x$extra.args))
}
