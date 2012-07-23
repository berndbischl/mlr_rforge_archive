#fixme: document args

#' Fuse learner with tuning.
#' 
#' Fuses a base learner with a search strategy to select its hyperparameters. Creates a learner object, which can be
#' used like any other learner object, but which internally uses tune. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set of hyperparameter values. Finally, a model is fitted on the complete training data with these optimal
#' hyperparameters and returned.    
#' See \code{\link{tune}} for more details.
#' 
#' @param learner [\code{\link{Learner}} or string]\cr 
#'   Learning algorithm.   
#' @param resampling [\code{\link{ResampleInstance}}] or [\code{\link{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param measures [list of \code{\link{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}] \cr
#'   Collection of parameters and their constraints for optimization.   
#' @param control [\code{\link{TuneControl}}] \cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.   
#' @param log.fun [function()]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function
#'   is optimized during tuning, others are simply evaluated.  
#' @return \code{\linkS4class{Learner}}. 
#' @export
makeTuneWrapper = function(learner, resampling, measures, par.set, control, show.info) {
  if (missing(measures))
    measures = mlr:::default.measures(learner)
  if (is(measures, "Measure"))
    measures = list(measures)   
	x = makeOptWrapper(learner, resampling, measures, par.set, character(0),
    function(){}, control, show.info)
  mlrTune:::checkTunerParset(learner, par.set, control) 
  return(x)
}

#' @S3method trainLearner TuneWrapper
trainLearner.TuneWrapper = function(.learner, .task, .subset,  ...) {
  # fixme: strange error if we remove :::? maybe rename subset...
  task = subsetData(.task, .subset)
    or = tune(.learner$learner, task, .learner$resampling, .learner$measures, 
              .learner$opt.pars, .learner$control)
    # set optimal hyper pars in base learner
    .learner$learner = setHyperPars(.learner$learner, par.vals=or$x)
    m = train(.learner$learner, task)
  # set the opt result as attribute, so we can extract it later 
  attr(m, "opt.result") = or
  return(m)
}
