#' Control structure for SPO optimization. 
#' @exportClass SPOControl
#' @seealso \code{\link{makeSPOControl}}
setClass(
  "SPOControl",
  representation = representation(
    y.name = "character",
    minimize = "logical",
    init.design.points = "integer", 
    init.design.fun = "function", 
    init.design.args = "list",
    seq.loops = "integer", 
    propose.points = "integer",
    propose.points.method = "character",
    seq.design.points = "integer", 
    seq.design.fun = "function", 
    seq.design.args = "list",
    rank.trafo = "logical",
    final.point = "character",
    final.evals = "integer",
    save.model.at = "integer",
    resample.desc = "ResampleDesc",
    resample.at = "integer",
    resample.measures = "list"
  )
)

#' Creates a control object for SPO optimization.
#'
#' @param y.name [\code{character(1)}]\cr 
#'   Name of y-column for target values in optimization path. Default is 'y'.   
#' @param minimize [logical(1)]\cr 
#'   Should target function be minimized? Default is \code{TRUE}.   
#' @param init.design.points [integer(1)]\cr 
#'   Number of points in inital design. 
#'   Only used if no design is given in \code{spo} function. Default is 20.   
#' @param init.design.fun [function] \cr
#'   Function from package lhs for the sequentail design. Possible are: maximinLHS, randomLHS, geneticLHS, improvedLHS, , optAugmentLHS, optimumLHS.
#'   Only used if no design is given in \code{spo} function. Default is 'randomLHS'. 
#' @param init.design.args [list] \cr
#'   List of further arguments passed to \code{init.design.fun}.  
#'   Only used if no design is given in \code{spo} function. Default is empty list.
#' @param seq.loops [integer(1)]\cr 
#'   Number of sequential optimization steps. Default is 100.   
#' @param propose.points [integer(1)]\cr 
#'   Number of proposed points after optimizing the surrogate model with \code{propose.points.methods}.   
#' @param propose.points.method [\code{character(1)}]\cr 
#'   How should points be proposed by using the surrogate model. Possible are: 
#'   'seq.design': Use a large design of points and evaluate the surrogate model at each. The best \code{propose.points} are selected.    
#'   'CMAES': Use CMAES to optimize mean prediction value.    
#'   'EI': Use expected improvement.    
#' @param seq.design.points [integer(1)]\cr 
#'   Number of points in sequential design. Only used if \code{propose.points.method} is 'seq.design.' Default is 10000.   
#' @param seq.design.fun [function] \cr
#'   Function from package lhs for the sequentail design. Possible are: maximinLHS, randomLHS, geneticLHS, improvedLHS, , optAugmentLHS, optimumLHS.
#'   Only used if \code{propose.points.method} is 'seq.design.' Default is 'randomLHS'. 
#' @param seq.design.args [list] \cr
#'   List of further arguments passed to \code{seq.design.fun}.  
#'   Only used if \code{propose.points.method} is 'seq.design.' Default is empty list.
#' @param rank.trafo [logical(1)]\cr 
#'   Should target values be rank-transformed before they are passed to the regresson model? 
#'   Default is \code{FALSE}.   
#' @param final.point [\code{character(1)}]\cr 
#'   How should the final point be proposed. Possible are: 
#'   'last.proposed': Return the last point proposed by the model.    
#'   'best.true.y': Return best point ever visited according to true value of target function. Can be bad if target function is noisy.    
#'   'best.predicted': Use the final model to predict all points ever visited and use the best one. This might average-out noisy function values.
#'   Default is: 'last.proposed'.     
#' @param final.evals [integer(1)]\cr 
#'   How many target function evals should be done at final point to reduce noise? 
#'   Default is 20.      
#' @param save.model.at [\code{integer}] \cr
#'   Sequential optimzation iterations when the model should be saved. Iteration 0 is the model fit for the initial design.
#'   Default is \code{seq.loops}.
#' @param resample.at [\code{integer}] \cr
#'   At which iterations should the model be resampled and assessed?
#'   Default is none.
#' @param resample.desc [\code{\linkS4class{ResampleDesc}}] \cr
#'   How should be model be resampled? 
#'   Default is 10-fold CV.
#' @param resample.measures [list of \code{\linkS4class{Measure}}]\cr
#'   Performance measures to assess model with during resampling. 
#'   Default is mse.   
#' @return The control object.  
#' @export 
makeSPOControl = function(y.name="y", minimize=TRUE,
  init.design.points=20, init.design.fun=maximinLHS, init.design.args=list(),
  seq.loops=100, propose.points=1, propose.points.method="seq.design", 
  seq.design.points=10000, seq.design.fun=randomLHS, seq.design.args=list(),
  rank.trafo=FALSE, final.point = "last.proposed",
  final.evals = 20,
  save.model.at = seq.loops,
  resample.at = integer(0), resample.desc = makeResampleDesc("CV", iter=10), resample.measures=list(mse) 
) {
  require.packs("lhs", "makeSPOControl")
  check.arg(y.name, "character", 1)
  check.arg(propose.points.method, "character", 1, c("seq.design", "CMAES", "EI"))
  
  if (is.numeric(init.design.points) && length(init.design.points) == 1 && as.integer(init.design.points) == init.design.points)
    init.design.points = as.integer(init.design.points)
  if (is.numeric(seq.loops) && length(seq.loops) == 1 && as.integer(seq.loops) == seq.loops)
    seq.loops = as.integer(seq.loops)
  if (is.numeric(propose.points) && length(propose.points) == 1 && as.integer(propose.points) == propose.points)
    propose.points = as.integer(propose.points)
  if (is.numeric(seq.design.points) && length(seq.design.points) == 1 && as.integer(seq.design.points) == seq.design.points)
    seq.design.points = as.integer(seq.design.points)
  check.arg(rank.trafo, "logical", 1)
  if (is.numeric(final.evals) && as.integer(final.evals) == final.evals)
    final.evals = as.integer(final.evals)
  if (length(save.model.at) == 0 || 
    (is.numeric(save.model.at) && as.integer(save.model.at) == save.model.at))
    save.model.at = as.integer(save.model.at)
  check.arg(save.model.at, "integer")
  check.arg(final.point, "character", 1, c("last.proposed", "best.true.y", "best.predicted"))
  check.arg(final.evals, "integer", 1)
  if (length(resample.at) == 0 || 
    (is.numeric(resample.at) && as.integer(resample.at) == resample.at))
    resample.at = as.integer(resample.at)
  check.arg(resample.at, "integer")
  check.arg(resample.desc, "ResampleDesc")
  
  new("SPOControl", 
    y.name = y.name,
    minimize = minimize,
    init.design.points = init.design.points, 
    init.design.fun = init.design.fun, 
    init.design.args = init.design.args,
    seq.loops = seq.loops, 
    propose.points = propose.points,
    propose.points.method = propose.points.method,
    seq.design.points = seq.design.points, 
    seq.design.fun = seq.design.fun, 
    seq.design.args = seq.design.args,
    rank.trafo = rank.trafo,
    final.point = final.point,
    final.evals = final.evals,
    save.model.at = save.model.at,
    resample.desc = resample.desc,
    resample.at = resample.at,
    resample.measures = resample.measures
  )
}

