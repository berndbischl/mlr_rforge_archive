#' @include control.tune.r
roxygen()

#' @exportClass spot.control
#' @rdname spot.control 

setClass(
  "spot.control",
  contains = c("tune.control")
)


#' Control structure for CMA-ES tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is \code{TRUE}.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param start [numeric] \cr
#'    Named vector of initial values.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[spot]{spot}}.
#'        
#' @return Control structure for tuning.
#' @exportMethod spot.control
#' @rdname spot.control 
#' @title Control for CMA-ES tuning. 


setGeneric(
  name = "spot.control",
  def = function(path, par.set, ...) {
    if (missing(path))
      path = TRUE
    
    #todo: convencience!!
    
    standardGeneric("spot.control")
  }
)


#' @rdname spot.control 

setMethod(
  f = "spot.control",
  signature = signature(path="logical", same.resampling.instance="logical", par.set="list"),
  def = function(path, same.resampling.instance, par.set, ...) {
    new("spot.control", path=path, same.resampling.instance=same.resampling.instance,
      start=list(), par.set=par.set, ...)
  }
)

