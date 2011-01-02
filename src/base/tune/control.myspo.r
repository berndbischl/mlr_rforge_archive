#' @include control.tune.r
roxygen()

#' @exportClass myspo.control
#' @rdname myspo.control 

setClass(
  "myspo.control",
  contains = c("tune.control"),
  representation = representation(
    meta.learner = "learner",
    constr.learner = "learner",
    init.des.points = "integer",
    seq.des.points = "integer",
    seq.loops = "integer"
  )
)

setMethod(
  f = "initialize",
  signature = signature("myspo.control"),
  def = function(.Object, path, par.descs, scale, 
    meta.learner, init.des.points, seq.des.points, seq.loops,...) {
    .Object@meta.learner = meta.learner
    .Object@init.des.points = init.des.points
    .Object@seq.des.points = seq.des.points
    .Object@seq.loops = seq.loops
    .Object = callNextMethod(.Object=.Object, path, start=list(), par.descs, scale, ...)
    return(.Object)
  }
)

#' Control structure for CMA-ES tuning. 
#' 
#' @param path [boolean]\cr
#'   Should optimization path be saved? Default is TRUE.
#' @param start [numeric] \cr
#'    Named vector of initial values.
#' @param lower [numeric] \cr
#'    Named vector of lower boundary constraints. Default is -Inf. 
#' @param upper [numeric] \cr
#'    Named vector of upper boundary constraints. Default is Inf. 
#' @param scale [\code{\link{function}}] \cr 
#'    A function to scale the hyperparameters. E.g. maybe you want to optimize in some log-space.
#'    Has to take a vector and return a scaled one. Default is identity function.
#' @param ... Further control parameters passed to the \code{control} argument of \code{\link[myspo]{myspo}}.
#'        
#' @return Control structure for tuning.
#' @exportMethod myspo.control
#' @rdname myspo.control 
#' @title Control for CMA-ES tuning. 


setGeneric(
  name = "myspo.control",
  def = function(path, par.descs, scale, 
    meta.learner, init.des.points, seq.des.points, seq.loops, ...) {
    if (missing(path))
      path = TRUE
    if (missing(scale))
      scale=identity
    if (missing(meta.learner))
      meta.learner="regr.rpart"
    if (is.character(meta.learner))
      meta.learner = make.learner(meta.learner)
    if (missing(init.des.points))
      init.des.points = 5L
    if (missing(seq.des.points))
      seq.des.points = 5L
    if (missing(seq.loops))
      seq.loops = 5L
    standardGeneric("myspo.control")
  }
)


#' @rdname myspo.control 

setMethod(
  f = "myspo.control",
  signature = signature(minimize="logical", path="logical", par.descs="list", scale="function",
    meta.learner="learner", init.des.points="integer", seq.des.points="integer", seq.loops="integer"),
  def = function(minimize, path, par.descs, scale,
    meta.learner, init.des.points, seq.des.points, seq.loops, ...) {
    new("myspo.control", minimize=minimize, path=path,
      par.descs=par.descs, scale=scale, 
      meta.learner=meta.learner, init.des.points=init.des.points, seq.des.points=seq.des.points, seq.loops=seq.loops, ...)
  }
)

