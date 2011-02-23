#' @include TuneControl.R
roxygen()

#' @exportClass TuneSPOControl
#' @rdname TuneSPOControl 

setClass(
  "TuneSPOControl",
  contains = c("TuneControl"),
  representation = representation(
    spo.control = "SPOControl"
  )

)


#' Control structure for SPO tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [numeric] \cr
#'   Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[cmaes]{cma_es}}.
#'        
#' @return Control structure for tuning.
#' @exportMethod makeTuneSPOControl
#' @rdname makeTuneSPOControl 
#' @title Control for SPO tuning. 


setGeneric(
  name = "makeTuneSPOControl",
  def = function(path, same.resampling.instance, start, ...) {
    if (missing(path))
      path = TRUE
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    if (missing(start))
      stop("You have to provide a start value!")
    standardGeneric("makeTuneSPOControl")
  }
)


#' @rdname TuneSPOControl 

setMethod(
  f = "makeTuneSPOControl",
  signature = signature(path="logical", same.resampling.instance="logical", start="numeric"),
  def = function(path, same.resampling.instance, start, ...) {
    new("TuneSPOControl", path=path, same.resampling.instance=same.resampling.instance, start=as.list(start), ...)
  }
)

