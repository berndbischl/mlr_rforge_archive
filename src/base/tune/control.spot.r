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
#'   Should optimization path be saved? Default is TRUE.
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
  def = function(path, par.descs, ...) {
    if (missing(path))
      path = TRUE
    
    #todo: convencience!!
    
    standardGeneric("spot.control")
  }
)


#' @rdname spot.control 

setMethod(
  f = "spot.control",
  signature = signature(path="logical", par.descs="list"),
  def = function(path, par.descs, ...) {
    new("spot.control", path=path,
      start=list(), par.descs=par.descs, ...)
  }
)

