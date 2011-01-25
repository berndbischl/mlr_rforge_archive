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
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'   Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space. At least for grid search, if you pass a description, 
#'   it is instantiated at one, so all points are evaluated on the same training/test sets.	
#' @param control [\code{\linkS4class{tune.control}}] \cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.   
#' @param measure [\code{\linkS4class{measure}}]\cr
#'   Performance measure to optimize. 
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
	if (missing(measure))
		measure = default.measures(task)[[1]]
  cl = as.character(class(control))
	optim.func = switch(cl,
			grid.control = tune.grid,
#			pattern = tune.ps,
			cmaes.control = tune.cmaes,
			optim.control = tune.optim,
      diceoptim.control = tune.diceoptim,
      stop(paste("Tuning algorithm for", cl, "does not exist!"))
	)		
	
	if (missing(control)) {
		stop("You have to pass a control object!")
	}
	
	#.mlr.local$n.eval <<- 0
	
	opt.path = new("opt.path", x.names=sapply(bounds@pars, function(p) p@id), y.names=measure@id)
  or = optim.func(learner, task, resampling, measure, bounds, control, opt.path)

	
	or@opt$par = .mlr.scale.vals(or@opt$par, bounds)
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

.mlr.scale.val <- function(v, bounds) {
  mapply(function(par, x) par@scale(x), bounds@pars, v)
}

