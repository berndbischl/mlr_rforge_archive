#' Create control structures for feature selection.
#' 
#' The following methods are available:
#'
#' \describe{
#'   \item{VarselControlExhaustive}{Exhaustive search. All feature sets (up to a certain size) are searched.}
#'   \item{VarselControlRandom}{Random search. Features vectors are randomly drawn.}
#'   \item{VarselControlSequential}{Deterministic forward or backward search.}
#' }
#' 
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is \code{TRUE}.
#' @return [\code{\link{FeatSelControl}}]. The specific subclass is one of
#'   \code{\link{FeatSelControlExhaustive}}, \code{\link{FeatSelControlRandom}}, \code{\link{FeatSelControlSequential}}.
#' @name FeatSelControl
#' @rdname FeatSelControl
#' @aliases FeatSelControlExhaustive FeatSelControlRandom FeatSelControlSequential
NULL

makeFeatSelControl = function(same.resampling.instance, maxit, max.features, ..., cl) {
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  maxit = convertInteger(maxit)
  checkArg(maxit, "integer", len=1, min=1, na.ok=FALSE)
  max.features = convertInteger(max.features)
  checkArg(max.features, "integer", len=1, min=1, na.ok=FALSE)
	x = mlrTune:::makeOptControl(same.resampling.instance=same.resampling.instance)
  x$maxit = maxit
  x$max.features = max.features
  class(x) = c(cl, "FeatSelControl", class(x))
  return(x)
}

#S3method print FeatSelControl
print.FeatSelControl = function(x, ...) {
  catf("FeatSel control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  if (x$max.features == .Machine$integer.max)
    catf("Max. features: %i", x$max.features)
  else  
    catf("Max. features: <not used>")
  catf("Max. iterations: %i", x$maxit)
  #catf("Further arguments: %s", listToShortString(x$extra.args))
}


