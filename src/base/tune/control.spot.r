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
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE. 
#' @param path [boolean]\cr
#'        Should optimization path be saved?
#' @param start [numeric] \cr
#'    Named vector of initial values.
#' @param lower [numeric] \cr
#'    Named vector of lower boundary constraints. Default is -Inf. 
#' @param upper [numeric] \cr
#'    Named vector of upper boundary constraints. Default is Inf. 
#' @param scale [\code{\link{function}}] \cr 
#'    A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'    Has to take a vector and return a scaled one. Default is identity function.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[spot]{spot}}.
#'        
#' @return Control structure for tuning.
#' @exportMethod spot.control
#' @rdname spot.control 
#' @title Control for CMA-ES tuning. 


setGeneric(
  name = "spot.control",
  def = function(minimize, path, par.descs, scale, ...) {
    if (missing(minimize))
      minimize=TRUE
    if (missing(path))
      path = FALSE
    
    #todo: convencience!!
    
#    if (missing(lower))
#    {lower=start;lower[]=-Inf}  
#    if (length(lower)==1)
#      lower = rep(lower, length(start))
#    if (is.null(names(lower)))
#      names(lower) = names(start)
#    if (missing(upper))
#    {upper=start;upper[]=Inf}       
#    if (length(upper)==1)
#      upper = rep(upper, length(start))
#    if (is.null(names(upper)))
#      names(upper) = names(start)
    if (missing(scale))
      scale=identity
    standardGeneric("spot.control")
  }
)


#' @rdname spot.control 

setMethod(
  f = "spot.control",
  signature = signature(minimize="logical", path="logical", par.descs="list", scale="function"),
  def = function(minimize, path, par.descs, scale, ...) {
    new("spot.control", minimize=minimize, path=path,
      start=list(), par.descs=par.descs, scale=scale, ...)
  }
)

