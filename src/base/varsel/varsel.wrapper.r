#' Fuses a base learner with a search strategy to select variables. Creates a learner object, which can be
#' used like any other learner object, but which internally uses varsel. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set of variables. Finally, a model is fitted on the complete training data with these variables and returned.    
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param control [\code{\linkS4class{varsel.control}}] 
#'   Control object for search method. Also selects the optimization algorithm for feature selection. 
#' @param measures [list of \code{\linkS4class{measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' @param log.fun [function(learner, task, resampling, measure, par.set, control, opt.path, x, y)]\cr
#'   Called after every hyperparameter evaluation. Default is to print performance via mlr logger. 
#' 
#' @return \code{\linkS4class{learner}}.
#' @export
#' @seealso \code{\link{varsel}}, \code{\link{varsel.control}} 
#' @title Fuse learner with variable selection.

make.varsel.wrapper = function(learner, resampling, measures, control, log.fun) {
  if (missing(log.fun))
    log.fun = log.fun.varsel
  make.opt.wrapper(learner, resampling, measures, makeParameterSet(), control, log.fun)
}
