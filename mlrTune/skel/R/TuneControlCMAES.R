#' Control structure for CMA-ES tuning. 
#' @exportClass TuneControlCMAES
#' @seealso \code{\link{makeTuneControlCMAES}}


#' Create control structure for CMA-ES tuning. 
#' 
#' @title Control for CMA-ES tuning. 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [\code{numeric}]\cr
#'   Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[cmaes]{cma_es}}.
#' @return [\code{\linkS4class{TuneControlCMAES}}].
#' @export
makeTuneControlCMAES = function(path=TRUE, same.resampling.instance=TRUE, start, ...) {
  checkArg(path, "logical", len=1, na.ok=FALSE)
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  checkArg(start, "numeric")
  makeTuneControlOptim(path=path, same.resampling.instance=same.resampling.instance, start=as.list(start), ...)
}

