#' Creates a control object for MBO optimization.
#'
#' @param y.name [\code{character(1)}]\cr 
#'   Name of y-column for target values in optimization path. 
#'   Default is \dQuote{y}.   
#' @param minimize [\code{logical(1)}]\cr 
#'   Should target function be minimized? 
#'   Default is \code{TRUE}.
#' @param impute [\code{function(x, y, opt.path)}]\cr
#'   Function that determines the return value in case the original fitness functions fails
#'   (for whatever reason) and because of this failure returns a NA, NaN, Inf.
#'   \code{x} is the current x-value, \code{y} the current (infeasible) y-value and 
#'   \code{opt.path} the current optimization path. 
#'   Default is to stop with an error.     
#' @param impute.errors [\code{logical(1)}]\cr
#'   Should fitness function call be wrapped in a \code{try} and the same imputation
#'   be used as in \code{impute}?
#'   Default is \code{FALSE}.
#' @param silent [\code{logical(1)}]\cr
#'   Should reporting of error messages during target function evaluations be suppressed? 
#'   Only used if \code{impute.errors} is \code{TRUE}.
#'   Default is \code{TRUE}.
#' @param init.design.points [\code{integer(1)}]\cr 
#'   Number of points in inital design. 
#'   Only used if no design is given in \code{mbo} function.
#'   Default is 20.   
#' @param init.design.fun [\code{function}]\cr
#'   Function from package lhs for the sequentail design. 
#'   Possible are: \code{maximinLHS}, \code{randomLHS}, \code{geneticLHS}, 
#'   \code{improvedLHS}, \code{optAugmentLHS}, \code{optimumLHS}.
#'   Only used if no design is given in \code{mbo} function. 
#'   Default is \code{randomLHS}. 
#' @param init.design.args [\code{list}]\cr
#'   List of further arguments passed to \code{init.design.fun}.  
#'   Only used if no design is given in \code{mbo} function. 
#'   Default is empty list.
#' @param seq.loops [\code{integer(1)}]\cr 
#'   Number of sequential optimization steps. 
#'   Default is 100.   
#' @param propose.points [\code{integer(1)}]\cr 
#'   Number of proposed points after optimizing the surrogate model with \code{infill.opt}.   
#'   Default is 1.
#' @param infill.crit [\code{character(1)}]\cr
#'   How should infill points be rated. Possible parameter values are:
#'   \dQuote{mean}: Mean response.
#' @param infill.opt [\code{character(1)}]\cr 
#'   How should points be proposed by using the surrogate model. Possible are: 
#'   \dQuote{design}: Use a large design of points and evaluate the surrogate model at each. 
#'    The best \code{propose.points} are selected.    
#'   \dQuote{CMAES}: Use CMAES to optimize mean prediction value.    
#'   \dQuote{EI}: Use expected improvement.    
#'   Default is \dQuote{design}.
#' @param seq.design.points [\code{integer(1)}]\cr 
#'   Number of points in sequential design. Only used if \code{infill.opt} is 'design.' 
#'   Default is 10000.   
#' @param seq.design.fun [\code{function}]\cr
#'   Function from package lhs for the sequentail design. 
#'   Possible are: \code{maximinLHS}, \code{randomLHS}, \code{geneticLHS}, 
#'   \code{improvedLHS}, \code{optAugmentLHS}, \code{optimumLHS}.
#'   Only used if \code{infill.opt} is \dQuote{design}.
#'   Default is \code{randomLHS}. 
#' @param seq.design.args [\code{list}]\cr
#'   List of further arguments passed to \code{seq.design.fun}.  
#'   Only used if \code{infill.opt} is \dQuote{design}. 
#'   Default is empty list.
#' @param final.point [\code{character(1)}]\cr 
#'   How should the final point be proposed. Possible are:    
#'   \dQuote{best.true.y}: Return best point ever visited according to true value of target function. Can be bad if target function is noisy.    
#'   \dQuote{last.proposed}: Return the last point proposed by the model.
#'   \dQuote{best.predicted}: Use the final model to predict all points ever visited and use the best one. This might average-out noisy function values.
#'   Default is: \dQuote{best.true.y}.     
#' @param final.evals [\code{integer(1)}]\cr 
#'   How many target function evals should be done at final point to reduce noise? 
#'   Default is 0.      
#' @param save.model.at [\code{integer}]\cr
#'   Sequential optimization iterations when the model should be saved. 
#'   Iteration 0 is the model fit for the initial design.
#'   Default is \code{seq.loops}.
#' @param resample.at [\code{integer}]\cr
#'   At which iterations should the model be resampled and assessed?
#'   Default is none.
#' @param resample.desc [\code{\link[mlr]{ResampleDesc}}]\cr
#'   How should be model be resampled? 
#'   Default is 10-fold CV.
#' @param resample.measures [list of \code{\link[mlr]{Measure}}]\cr
#'   Performance measures to assess model with during resampling. 
#'   Default is \code{\link[mlr]{mse}}.   
#' @return [\code{\link{MBOControl}}].
#' @aliases MBOControl 
#' @export 
makeMBOControl = function(y.name="y", minimize=TRUE,
  impute, impute.errors=FALSE, silent=TRUE,
  init.design.points=20, init.design.fun=maximinLHS, init.design.args=list(),
  seq.loops=100, propose.points=1,
  infill.crit="mean", infill.opt="design",
  seq.design.points=10000, seq.design.fun=randomLHS, seq.design.args=list(),
  cmaes.control = list(),                          
  final.point = "best.true.y",
  final.evals = 0,
  save.model.at = seq.loops,
  resample.at = integer(0), resample.desc = makeResampleDesc("CV", iter=10), resample.measures=list(mse) 
) {
  
  requirePackages("lhs", "makeMBOControl")
  
  checkArg(y.name, "character", len=1L, na.ok=FALSE)
  checkArg(minimize, "logical", len=1L, na.ok=FALSE)
  
  checkArg(infill.crit, choices=c("mean", "ei", "aei"))
  checkArg(infill.opt, choices=c("design", "cmaes"))
  
  if (missing(impute)) 
    impute = function(x, y, opt.path) 
      stopf("Infeasible y=%s value encountered at %s", as.character(y), listToShortString(x))
  else 
    checkArg(impute, formals=c("x", "y", "opt.path"))
  checkArg(impute.errors, "logical", len=1L, na.ok=FALSE)
  checkArg(silent, "logical", len=1L, na.ok=FALSE)
  
  init.design.points = convertInteger(init.design.points)
  checkArg(init.design.points, "integer", len=1L, na.ok=FALSE, lower=4L)
  checkArg(init.design.fun, "function")
  checkArg(init.design.args, "list")
  
  seq.loops = convertInteger(seq.loops)
  checkArg(seq.loops, "integer", len=1L, na.ok=FALSE, lower=1L)
  propose.points = convertInteger(propose.points)
  checkArg(propose.points, "integer", len=1L, na.ok=FALSE, lower=1L)
  seq.design.points = convertInteger(seq.design.points)
  checkArg(seq.design.points, "integer", len=1L, na.ok=FALSE, lower=1L) 
  checkArg(seq.design.fun, "function")
  checkArg(seq.design.args, "list")
  # FIXME: remove this for now
  #checkArg(rank.trafo, "logical", len=1L, na.ok=FALSE)
  final.evals = convertInteger(final.evals)
  checkArg(final.evals, "integer", len=1L, na.ok=FALSE, lower=0L)
  # FIXME: do better
  if (length(save.model.at) > 0) {
    save.model.at = convertIntegers(save.model.at)
    checkArg(save.model.at, "integer", na.ok=FALSE, lower=0L, upper=seq.loops)
  } else {
    save.model.at = integer(0)
  }
  checkArg(final.point, choices=c("last.proposed", "best.true.y", "best.predicted"))
  checkArg(final.evals, "integer", len=1L, na.ok=FALSE)
  if (length(resample.at) > 0) {
    resample.at = convertIntegers(resample.at)
    checkArg(resample.at, "integer", na.ok=FALSE, lower=0L, upper=seq.loops)
  } else {
    resample.at = integer(0)
  }
  checkArg(resample.desc, "ResampleDesc")
  checkArg(resample.measures, "list")
  
  structure(list( 
    y.name = y.name,
    minimize = minimize,
    impute = impute,
    impute.errors = impute.errors,
    silent = silent,
    init.design.points = init.design.points, 
    init.design.fun = init.design.fun, 
    init.design.args = init.design.args,
    infill.crit = infill.crit,
    infill.opt = infill.opt,
    cmaes.control = cmaes.control,
    seq.loops = seq.loops, 
    propose.points = propose.points,
    infill.opt = infill.opt,
    seq.design.points = seq.design.points, 
    seq.design.fun = seq.design.fun, 
    seq.design.args = seq.design.args,
    #rank.trafo = rank.trafo,
    final.point = final.point,
    final.evals = final.evals,
    save.model.at = save.model.at,
    resample.desc = resample.desc,
    resample.at = resample.at,
    resample.measures = resample.measures
  ), class= "MBOControl")
}

# Print mbo control object.
# 
# @param x [\code{\link{MBOControl}}]\cr
#   Control object.
# @param ... [any]\cr
#   Not used.
#' @S3method print MBOControl
print.MBOControl = function(x, ...) {
  minmax = ifelse(x$minimize, "min", "max")
  catf("Objective         : %s = %s!", x$y.name, minmax)
  catf("Init. design      : %i points", x$init.design.points)
  catf("Iterations        : %i", x$seq.loops)
  catf("Infill criterion  : %s", x$infill.crit)
  catf("Infill optimzer   : %s", x$infill.opt)
  catf("Final point by    : %s", x$final.point)
}

