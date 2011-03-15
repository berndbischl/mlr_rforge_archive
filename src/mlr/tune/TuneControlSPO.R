#' @include TuneControl.R
roxygen()

#' Control structure for SPO tuning. 
#' @exportClass TuneControlSPO
#' @seealso \code{\link{makeTuneControlSPO}}

setClass(
  "TuneControlSPO",
  contains = c("TuneControl"),
  representation = representation(
    learner = "Learner",
    spo.control = "SPOControl"
  )

)
#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("TuneControlSPO"),
  def = function(.Object, path, same.resampling.instance, learner, spo.control) {
    .Object@learner = learner  
    .Object@spo.control = spo.control  
    callNextMethod(.Object, path, same.resampling.instance, start=list())
  }
)

#' Create control structure for SPO tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'   Regression learner to model performance landscape.  
#' @param control [\code{\linkS4class{SPOControl}}] \cr
#'   Control object for SPO.  
#'        
#' @return Control structure for tuning.
#' @exportMethod makeTuneControlSPO
#' @rdname makeTuneControlSPO 
#' @title Control for SPO tuning. 


setGeneric(
  name = "makeTuneControlSPO",
  def = function(path, same.resampling.instance, learner, spo.control) {
    if (missing(path))
      path = TRUE
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    if (is.character(learner))
      learner = makeLearner(learner)
    standardGeneric("makeTuneControlSPO")
  }
)


#' @rdname makeTuneControlSPO 

setMethod(
  f = "makeTuneControlSPO",
  signature = signature(path="logical", same.resampling.instance="logical", 
    learner="Learner", spo.control="SPOControl"),
  def = function(path, same.resampling.instance, learner, spo.control) {
    new("TuneControlSPO", path=path, same.resampling.instance=same.resampling.instance,
      learner=learner, spo.control=spo.control)
  }
)

