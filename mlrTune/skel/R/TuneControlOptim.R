
#' Control structure for tuning with \code{optim}.
#' @exportClass TuneControlOptim
#' @seealso \code{\link{makeTuneControlOptim}}


#' Create control structure for tuning with optim (Nelder-Mead, SANN, etc). 
#' 
#' @title Control for tuning with optim. 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [\code{numeric}]\cr
#'		Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[stats]{optim}}.
#' @return [\code{\linkS4class{TuneControlOptim}}].
#' @export
makeTuneControlOptim = function(path=TRUE, same.resampling.instance=TRUE, start, ...) {
  checkArg(path, "logical", len=1, na.ok=FALSE)
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  checkArg(start, "numeric")
  makeTuneControlOptim(path=path, same.resampling.instance=same.resampling.instance, start=as.list(start), ...)
}

