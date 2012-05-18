#' @include TuneControl.R
roxygen()
#' @importClassesFrom mlrMBO MboControl
roxygen()

#' Control structure for Mbo tuning. 
#' @exportClass TuneControlMbo
#' @seealso \code{\link{makeTuneControlMbo}}

setClass(
  "TuneControlMbo",
  contains = c("TuneControl"),
  representation = representation(
    learner = "Learner",
    mbo.control = "MboControl"
  )

)
#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("TuneControlMbo"),
  def = function(.Object, path, same.resampling.instance, learner, mbo.control) {
    .Object@learner = learner  
    .Object@mbo.control = mbo.control  
    callNextMethod(.Object, path, same.resampling.instance, start=list())
  }
)

#' Create control structure for model-based optimization tuning. 
#' 
#' @title Control for model-based optimization tuning. 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'   Regression learner to model performance landscape.  
#' @param mbo.control [\code{\linkS4class{MboControl}}] \cr
#'   Control object for model-based optimization tuning.  
#' @return [\code{\linkS4class{TuneControlMbo}}].
#' @export
makeTuneControlMbo = function(path=TRUE, same.resampling.instance=TRUE, learner, mbo.control) {
  checkArg(path, "logical", len=1, na.ok=FALSE)
  checkArg(same.resampling.instance, "logical", len=1, na.ok=FALSE)
  checkArg(learner, "Learner")
  checkArg(mbo.control, "MboControl")
  new("TuneControlMbo", path=path, same.resampling.instance=same.resampling.instance,
    learner=learner, mbo.control=mbo.control)
}

