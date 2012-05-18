
#' Control structure for grid search tuning.
#' @exportClass TuneControlGrid
#' @seealso \code{\link{makeTuneControlGrid}}


#' Create control structure for grid search tuning. 
#' 
#' @title Control for grid search tuning. 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [\code{logical(1)}] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @return [\code{\linkS4class{TuneControlGrid}}].
#' @export
makeTuneControlGrid = function(path=TRUE, same.resampling.instance=TRUE) {
  checkArg(path, "logical", len=1, na.ok=FALSE)
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  makeTuneControlOptim(path=path, same.resampling.instance=same.resampling.instance, start=list())
}

