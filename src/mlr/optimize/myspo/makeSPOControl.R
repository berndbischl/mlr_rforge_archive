#todo: document resampling when sure that ok.
#' Creates a control object for SPO optimization.
#'
#' @param seq.loops [integer(1)]\cr 
#'   Number of sequential optimization steps. Default is 100.   
#' @param propose.points [integer(1)]\cr 
#'   Number of proposed points after optimizing the surrogate model with \code{propose.points.methods}.   
#' @param propose.points.method [character(1)]\cr 
#'   How should points be proposed by using the surrogate model. Possible are: 
#'   'seq.design': Use a large design of points and evaluate the surrogate model at each. The best \code{propose.points} are selected.    
#' @param seq.design.points [integer(1)]\cr 
#'   Number of points in sequential design. Only used if \code{propose.points.method} is 'seq.design.' Default is 10000.   
#' @param seq.design.points [integer(1)]\cr 
#'   Number of points in sequential design. Only used if \code{propose.points.method} is 'seq.design.' Default is 10000.   
#' @param seq.design.fun [function] \cr
#'   Function from package lhs for the sequentail design. Possible are: maximinLHS, randomLHS, geneticLHS, improvedLHS, , optAugmentLHS, optimumLHS.
#'   Only used if \code{propose.points.method} is 'seq.design.' Default is 'randomLHS'. 
#' @param seq.design.args [list] \cr
#'   List of further arguments passed to \code{seq.design.fun}.  
#'   Only used if \code{propose.points.method} is 'seq.design.' Default is empty list.
#' @param save.model.at [integer] \cr
#'   Vector of sequential optimzation iterations when the model should be saved. Iteration 1 is the model fit for the initial design.
#'   Default is \code{c(1, seq.loops)}.
#' @return The control object.  
#' @export 
makeSPOControl = function(seq.loops=100, propose.points=1, propose.points.method="seq.design", 
  seq.design.points=10000, seq.design.fun=randomLHS, seq.design.args=list(),
  resample.desc = make.res.desc("cv", iter=10), resample.at = c(1, seq.loops), resample.measures=list(mse) 
) {
  
  list( 
    seq.loops = seq.loops, 
    propose.points = propose.points,
    propose.points.method = propose.points.method,
    seq.design.points = seq.design.points, 
    seq.design.fun = seq.design.fun, 
    seq.design.points = seq.design.points,
    resample.desc = resample.desc,
    resample.at = resample.at,
    resample.measures = resample.measures
  )
}

