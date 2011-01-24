#' @include control.tune.r
roxygen()

#' @exportClass DiceOptim.control
#' @rdname DiceOptim.control 

setClass(
  "DiceOptim.control",
  contains = c("tune.control")
)


#' Control structure for EGO tuning with DiceOptim. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[DiceOptim]{optim}}.
#'        
#' @return Control structure for tuning.
#' @exportMethod DiceOptim.control
#' @rdname DiceOptim.control 
#' @title Control for tuning with DiceOptim. 


setGeneric(
  name = "DiceOptim.control",
  def = function(path, init.des.points, ...) {
    if (missing(path))
      path = TRUE
    if (missing(init.des.points))
      init.des.points = 5L
    standardGeneric("DiceOptim.control")
  }
)


#' @rdname myspo.control 

setMethod(
  f = "DiceOptim.control",
  signature = signature(path="logical", init.des.points="integer" ),
  def = function(path, init.des.points, ...) {
    new("DiceOptim.control", path=path, start=list(), init.des.points=init.des.points,  ...)
  }
)
