#fixme: document args

#' Fuse learner with tuning.
#' 
#' Fuses a base learner with a search strategy to select its hyperparameters. Creates a learner object, which can be
#' used like any other learner object, but which internally uses tune. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set of hyperparameter values. Finally, a model is fitted on the complete training data with these optimal
#' hyperparameters and returned.    
#' See \code{\link[mlrTune]{tune}} for more details.
#' 
#' @param learner [\code{\link[mlr]{Learner}} or string]\cr 
#'   Learning algorithm.   
#' @param resampling [\code{\link[mlr]{ResampleDesc}} | \code{\link[mlr]{ResampleInstance}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param measures [list of \code{\link[mlr]{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}] \cr
#'   Collection of parameters and their constraints for optimization.   
#' @param control [\code{\link{TuneControl}}] \cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.   
#' @param show.info [\code{logical(1)}]\cr
#'   Show info message after each hyperparameter evaluation?
#'   Default is \code{TRUE}.
#' @return [\code{\link{Learner}}]. 
#' @export
makeTuneWrapper = function(learner, resampling, measures, par.set, control, show.info=TRUE) {
  id = paste(learner$id, "tuned", sep=".")
	x = makeOptWrapper(id, learner, resampling, measures, par.set, character(0),
    function(){}, control, show.info, "TuneWrapper")
  mlrTune:::checkTunerParset(learner, par.set, control) 
  return(x)
}

#' @S3method trainLearner TuneWrapper
trainLearner.TuneWrapper = function(.learner, .task, .subset,  ...) {
  .task = subsetTask(.task, .subset)
  or = tune(.learner$next.learner, .task, .learner$resampling, .learner$measures, 
    .learner$opt.pars, .learner$control, .learner$show.info)
  lrn = setHyperPars(.learner$next.learner, par.vals=or$x)
  m = train(lrn, .task)
  x = makeChainModel(next.model=m, cl = "TuneModel")
  x$opt.result = or
  return(x)
}

#' @S3method predictLearner TuneWrapper
predictLearner.TuneWrapper = function(.learner, .model, .newdata, ...) {
  lrn = setHyperPars(.learner$next.learner, 
    par.vals=.model$learner.model$opt.result$x)
  predictLearner(lrn, .model$learner.model$next.model, .newdata)
}




