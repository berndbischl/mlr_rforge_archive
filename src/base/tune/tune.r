#' @include task.learn.r
roxygen()


#' Optimizes the hyperparameters of a learner for a classification or regression problem.
#' Allows for different optimization methods, commonly grid search is used but other search techniques
#' are available as well.
#' The specific details of the search algorithm are set by passing a control object.   
#'
#' @param learner [\code{\linkS4class{wrapped.learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'        Resampling strategy to evaluate points in hyperparameter space.
#' @param method [\code{\link{character}}] \cr
#'        Search method. Currently supported are "grid", "pattern", "cmaes".   
#' @param control 
#'        Control object for search method.   
#' @param loss [\code{\linkS4class{loss}}] or [\code{\link{character}}]\cr
#'        Loss to use for tuning. Default is "zero-one" for classification and "squared" error for regression.
#' @param model [\code{\link{logical}}]\cr
#'        Should a final model be fitted on the complete data with the best found hyperparameters?
#' @param scale [\code{\link{function}}]
#'        A function to scale the hyperparamters. E.g. maybe you want to optimize in some log-space.
#'        Has to take a single, numerical vector and return a scaled one. Default is identity function.
#' 
#' @return A list. Might contain some additional information from the optimizer and at least:
#'   \item{par}{Named list of best found hyperparamters.}
#'   \item{perf}{Best found performance value.}
#'   \item{model}{Fitted model on complete data set - if requested.}
#' 
#' @export
#'
#' @usage tune(learner, task, resampling, method="grid", control=NULL, loss, model=F, scale=identity)
#'  
#' @title Hyperparameter tuning


tune <- function(learner, task, resampling, method="grid", control=NULL, measures, aggr, model=F, scale=identity) {
	if (missing(measures))
		measures = default.measures(task)
	measures = make.measures(measures)
	if (missing(aggr))
		aggr = default.aggr(task)
	aggr = make.aggrs(aggr)
	
	if (method == "cmaes" && !require(cmaes)) {
		stop("You have to install the package cmaes for this!")
	}
	
	optim.func = switch(method,
			grid = tune.grid,
			pattern = tune.ps,
			cmaes = tune.cmaes,
			neldermead= tune.nm,
			stop(paste("Method", method, "does not exist!"))
	)		
	
	if (is.null(control)) {
		stop("You have to pass a control object!")
	}
	if ((method == "grid"       && !is(control, "grid.control")) ||
		(method == "pattern"    && !is(control, "ps.control")) ||
		(method == "cmaes"       && !is(control, "cmaes.control")) ||
		(method == "neldermead" && !is(control, "nm.control"))) {
			stop(paste("Method is '", method, "'. You have passed a control object of the wrong type: ", class(control), sep=""))
	}
	assign(".mlr.feval", 0, envir=.GlobalEnv)
	
	#.mlr.local$n.eval <<- 0
	#export.tune(learner, task, loss, scale)
	or = optim.func(learner=learner, task=task, resampling=resampling, measures=measures, aggr=aggr, control=control, scale=scale)
	or$par = as.list(or$par)
	or$par = scale.par(scale, or$par)
	#or$n.eval = .mlr.local$n.eval
	if (model) {
		or$model = train(learner, task, parset=or$par) 	
	}
	
	return(or)			
}


scale.par <- function(f, p) {
	if (identical(f, identity))
		return(as.list(p))
	else
		return(as.list(f(unlist(p))))
	
}

