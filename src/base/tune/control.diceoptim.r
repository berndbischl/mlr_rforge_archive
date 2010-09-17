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
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE. 
#' @param tune.threshold [logical] \cr 
#'    Perform empirical thresholding? Default is FALSE. Only supported for binary classification and you have to set predict.type to "prob" for this in make.learner. 
#' @param thresholds [numeric] \cr 
#'    Number of thresholds to try in tuning. Predicted probabilities are sorted and divided into groups of equal size. Default is 10.             
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
  def = function(minimize, tune.threshold, thresholds, path, par.descs, scale,
    init.des.points, seq.loops, ...) {
    if (missing(minimize))
      minimize=TRUE
    if (missing(tune.threshold))
      tune.threshold=FALSE
    if (missing(thresholds))
      thresholds=10
    if (is.numeric(thresholds))
      thresholds = as.integer(thresholds)
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
  signature = signature(minimize="logical", tune.threshold="logical", thresholds="integer", path="logical", par.descs="list", scale="function",
    init.des.points="integer", seq.loops="integer"),
  def = function(minimize, tune.threshold, thresholds, path, par.descs, scale,
    init.des.points, seq.loops, ...) {
    new("DiceOptim.control", minimize=minimize, tune.threshold=tune.threshold, thresholds=thresholds, path=path,
      par.descs=par.descs, scale=scale, 
      meta.learner=meta.learner, init.des.points=init.des.points, seq.des.points=seq.des.points, seq.loops=seq.loops, ...)
  }
)
