#' @include TuneControl.R
roxygen()

#' Control structure for mbo tuning. 
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

#' Create control structure for Mbo tuning. 
#' 
#' @param path [\code{logical(1)}]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'   Regression learner to model performance landscape.  
#' @param control [\code{\linkS4class{MboControl}}] \cr
#'   Control object for Mbo.  
#'        
#' @return Control structure for tuning.
#' @exportMethod makeTuneControlMbo
#' @rdname makeTuneControlMbo 
#' @title Control for Mbo tuning. 


setGeneric(
  name = "makeTuneControlMbo",
  def = function(path, same.resampling.instance, learner, mbo.control) {
    if (missing(path))
      path = TRUE
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    if (is.character(learner))
      learner = makeLearner(learner)
    standardGeneric("makeTuneControlMbo")
  }
)


#' @rdname makeTuneControlMbo 

setMethod(
  f = "makeTuneControlMbo",
  signature = signature(path="logical", same.resampling.instance="logical", 
    learner="Learner", mbo.control="MboControl"),
  def = function(path, same.resampling.instance, learner, mbo.control) {
    new("TuneControlMbo", path=path, same.resampling.instance=same.resampling.instance,
      learner=learner, mbo.control=mbo.control)
  }
)

