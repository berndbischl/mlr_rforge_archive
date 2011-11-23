#todo: document args
#' Fuses a base learner with a search strategy to select its hyperparameters. Creates a learner object, which can be
#' used like any other learner object, but which internally uses tune. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set of hyperparameter values. Finally, a model is fitted on the complete training data with these optimal
#' hyperparameters and returned.    
#' See \code{\link{tune}} for more details.
#' 
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm.   
#' @param resampling [\code{\linkS4class{ResampleInstance}}] or [\code{\linkS4class{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param measures [list of \code{\linkS4class{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}] \cr
#'   Collection of parameters and their constraints for optimization.   
#' @param control [\code{\linkS4class{TuneControl}}] \cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.   
#' @param log.fun [function()]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' 
#' @return \code{\linkS4class{Learner}}.
#' 
#' @export
#'
#' @seealso \code{\link{tune}}, \code{\link{TuneControl}} 
#'   
#' @title Fuse learner with tuning.

makeTuneWrapper = function(learner, resampling, measures, par.set, control, log.fun) {
  if (is.character(learner))
    learner = makeLearner(learner)
  if (missing(measures))
    measures = mlr:::default.measures(learner)
  if (is(measures, "Measure"))
    measures = list(measures)   
  if (missing(log.fun))
    log.fun = mlrTune:::log.fun.tune
  mlrTune:::checkTunerParset(learner, par.set, control) 
	new("OptWrapper", learner, resampling, measures, par.set, character(0), function(){}, control, log.fun)
}

