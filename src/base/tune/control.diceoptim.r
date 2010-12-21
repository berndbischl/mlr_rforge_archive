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
#'        Should optimization path be saved?
#' @param lower [numeric] \cr
#'    Named vector of lower boundary constraints. Default is -Inf. 
#' @param upper [numeric] \cr
#'    Named vector of upper boundary constraints. Default is Inf. 
#' @param scale [\code{\link{function}}] \cr 
#'    A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'    Has to take a vector and return a scaled one. Default is identity function.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[DiceOptim]{optim}}.
#'        
#' @return Control structure for tuning.
#' @exportMethod DiceOptim.control
#' @rdname DiceOptim.control 
#' @title Control for tuning with DiceOptim. 


setGeneric(
  name = "DiceOptim.control",
  def = function(path, par.descs, scale,
    init.des.points, seq.loops, ...) {
    if (missing(path))
      path = FALSE
    if (missing(scale))
      scale=identity
    if (missing(init.des.points))
      init.des.points = 5L
    if (missing(seq.loops))
      seq.loops = 5L
    standardGeneric("DiceOptim.control")
  }
)


#' @rdname myspo.control 

setMethod(
  f = "DiceOptim.control",
  signature = signature(path="logical", par.descs="list", scale="function",
    init.des.points="integer", seq.loops="integer"),
  def = function(path, par.descs, scale,
    init.des.points, seq.loops, ...) {
    new("DiceOptim.control", path=path,
      par.descs=par.descs, scale=scale, 
      meta.learner=meta.learner, init.des.points=init.des.points, seq.des.points=seq.des.points, seq.loops=seq.loops, ...)
  }
)
