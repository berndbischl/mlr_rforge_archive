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
  def = function(path, lower, upper, scale, init.des.points, ...) {
    if (missing(path))
      path = TRUE
    if(!all.els.named(lower))
      stop("Argument lower has to be properly named!")
    if(!all.els.named(upper))
      stop("Argument upper has to be properly named!")
    if(!setequal(names(lower), names(upper)))
      stop("Arguments lower,upper must have the same names!")
    if (any(is.infinite(c(lower, upper)) | is.na(c(lower, upper))))
      stop("Arguments lower,upper must not be infinite or NA!")
    if (missing(scale))
      scale=identity
    if (missing(init.des.points))
      init.des.points = 5L
    standardGeneric("DiceOptim.control")
  }
)


#' @rdname myspo.control 

setMethod(
  f = "DiceOptim.control",
  signature = signature(path="logical", lower="numeric", upper="numeric", scale="function", init.des.points="integer" ),
  def = function(path, lower, upper, scale, init.des.points, ...) {
    pds = make.pds.from.lowup(names(lower), lower, upper)
    new("DiceOptim.control", path=path, par.descs=pds, start=list(), scale=scale, 
      init.des.points=init.des.points,  ...)
  }
)
