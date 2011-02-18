#' @include control.tune.r
roxygen()

#' @exportClass diceoptim.control
#' @rdname diceOptim.control 

setClass(
  "diceoptim.control",
  contains = c("tune.control")
)


#' Control structure for EGO tuning with DiceOptim. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param same.resampling.instance [logical(1)] \cr
#'    Should the same resampling instance be used for all evaluations to reduce variance? Default is \code{TRUE}.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[DiceOptim]{optim}}.
#'        
#' @return Control structure for tuning.
#' @exportMethod diceoptim.control
#' @rdname diceoptim.control 
#' @title Control for tuning with DiceOptim. 


setGeneric(
  name = "diceoptim.control",
  def = function(path, same.resampling.instance, init.des.points, ...) {
    if (missing(path))
      path = TRUE
    if (missing(same.resampling.instance))
      same.resampling.instance = TRUE
    if (missing(init.des.points))
      init.des.points = 5L
    standardGeneric("diceoptim.control")
  }
)


#' @rdname myspo.control 

setMethod(
  f = "diceoptim.control",
  signature = signature(path="logical", same.resampling.instance="logical", init.des.points="integer" ),
  def = function(path, same.resampling.instance, init.des.points, ...) {
    new("diceoptim.control", path=path, same.resampling.instance=same.resampling.instance, start=list(), init.des.points=init.des.points,  ...)
  }
)
