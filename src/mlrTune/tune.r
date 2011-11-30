#todo: check whether optimization can be paralleized if req. by user

#' Optimizes the hyperparameters of a learner for a classification or regression problem.
#' Allows for different optimization methods, such as grid search, evolutionary strategies 
#' or sequential parameter optimization. You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at the 
#' subclasses of [\code{\linkS4class{TuneControl}}].
#'
#' Note that if tranformations are associated with the parameters, the returned result will contain
#' a transformed optimal value, but an untransformed optimization path. 
#' See also \code{\link[ParamHelpers]{trafoValue}} and \code{\link[ParamHelpers]{trafoOptPath}}.
#' 
#' @title Hyperparameter tuning.
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{LearnTask}}]\cr
#'   Learning task.   
#' @param resampling [\code{\linkS4class{ResampleInstance}}] or [\code{\linkS4class{ResampleDesc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space. If you pass a description, 
#'   it is instantiated once at the beginning by default, so all points are evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at the control object. 	
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Collection of parameters and their constraints for optimization.   
#' @param control [\code{\linkS4class{TuneControl}}]\cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.   
#' @param measures [list of \code{\linkS4class{Measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' @param log.fun [function(learner, task, resampling, measure, par.set, control, opt.path, x, y)]\cr
#'   Called after every hyperparameter evaluation. Default is to print performance via mlr logger. 
#' @return \code{\linkS4class{OptResult}}.
#' @export
tune = function(learner, task, resampling, measures, par.set, control, log.fun) {
  if (is.character(learner))
    learner = makeLearner(learner)
  if (is(resampling, "ResampleDesc") && control@same.resampling.instance)
    resampling = makeResampleInstance(resampling, task=task)
	if (missing(measures))
		measures = mlr:::default.measures(task)
  if (is(measures, "Measure"))
    measures = list(measures)   
  if (missing(log.fun))
    log.fun = log.fun.tune
	if (missing(control)) {
		stop("You have to pass a control object!")
	}
  checkTunerParset(learner, par.set, control)  
  cl = as.character(class(control))[1]
	sel.func = switch(cl,
      TuneControlGrid = tune.grid,
#			pattern = tune.ps,
      TuneControlCMAES = tune.cmaes,
      TuneControlOptim = tune.optim,
      TuneControlMbo = tune.mbo
	)		
  opt.path = makeOptPathDFFromMeasures(par.set, measures)
  logger.info("[Tune] Started tuning learner", learner@id, "for parameter set:")
  sapply(capture.output(print(par.set)), logger.info)
  logger.info("with control class:",  cl)
  or = sel.func(learner, task, resampling, measures, par.set, control, opt.path, log.fun)
  logger.info("[Tune] Result:", paramValueToString(par.set, or@x), ":", perfsToString(or@y))
  # trafo the x value now
  or@x = trafoValue(par.set, or@x)
	return(or)			
}


