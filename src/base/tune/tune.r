#todo: check whether optimization can be paralleized if req. by user

#' @include task.learn.r
roxygen()


#todo instantiate resample description for others than grid search????

#' Optimizes the hyperparameters of a learner for a classification or regression problem.
#' Allows for different optimization methods, commonly grid search is used but other search techniques
#' are available as well.
#' The specific details of the search algorithm are set by passing a control object.   
#' 
#' The first measure, aggregated by the first aggregation function is optimized, to find a set of optimal hyperparameters.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'        Resampling strategy to evaluate points in hyperparameter space. At least for grid search, if you pass a description, 
#' 		  it is instantiated at one, so all points are evaluated on the same training/test sets.	
#' @param control [\code{\linkS4class{tune.control}}] \cr
#'        Control object for search method. Also selects the optimization algorithm for tuning.   
#' @param measures [see \code{\link{measures}}]\cr
#'        Performance measures. 
#' @param model [boolean]\cr
#'        Should a final model be fitted on the complete data with the best found hyperparameters? Default is FALSE.
#' 
#' @return \code{\linkS4class{opt.result}}.
#' 
#' @export
#'
#' @seealso \code{\link{grid.control}}, \code{\link{optim.control}}, \code{\link{cmaes.control}}
#'   
#' @title Hyperparameter tuning


tune <- function(learner, task, resampling, measure, bounds, control, log) {
  if (is.character(learner))
    learner <- make.learner(learner)
	if (missing(measures))
		measures = default.measures(task)
  if (is(measures, "measure"))
    measures = list(measures)   
  cl = as.character(class(control))
	optim.func = switch(cl,
			grid.control = tune.grid,
#			pattern = tune.ps,
			cmaes.control = tune.cmaes,
			optim.control = tune.optim,
      DiceOptim.control = tune.diceoptim,
      stop(paste("Tuning algorithm for", cl, "does not exist!"))
	)		
	
	if (missing(control)) {
		stop("You have to pass a control object!")
	}
	
	#.mlr.local$n.eval <<- 0
	#export.tune(learner, task, loss, scale)
	
	or = optim.func(learner=learner, task=task, resampling=resampling, control=control, measures=measures)

	
	or@opt$par = .mlr.scale.par(or@opt$par, control)
	if (model) {
    learner = set.hyper.pars(learner, par.vals=or["par"])
		or@model = train(learner, task) 	
	}
	
	return(or)			
}


#' Scale parameter vector. Internal use.
#' 
#' @export
#' @seealso \code{\link{tune.control}}
#' @title Scale parameter vector. Internal use.

.mlr.scale.par <- function(p, control) {
	sc = control["scale"]
	if (identical(sc, identity))
		y = as.list(p)
	else
		y = as.list(sc(unlist(p)))
	names(y) = control["par.names"]
	return(y)
}

