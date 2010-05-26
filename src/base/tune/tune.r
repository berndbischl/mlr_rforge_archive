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
#' @param method [\code{\link{character}}] \cr
#'        Search method. Currently supported are grid search "grid", pattern search "pattern", CMA-ES "cmaes" and Nelder-Mead "nm".   
#' @param control 
#'        Control object for search method.   
#' @param measures [see \code{\link{measures}}]\cr
#'        Performance measures. 
#' @param aggr [see \code{\link{aggregations}}]\cr
#'        Aggregation functions. 
#' @param model [\code{\link{logical}}]\cr
#'        Should a final model be fitted on the complete data with the best found hyperparameters?
#' 
#' @return \code{\linkS4class{opt.result}}.
#' 
#' @export
#'
#' @usage tune(learner, task, resampling, method="grid", control, measures, aggr, model=F)
#'
#' @seealso \code{\link{grid.control}}, \code{\link{ps.control}}, \code{\link{cmaes.control}}, \code{\link{nm.control}}
#'   
#' @title Hyperparameter tuning


tune <- function(learner, task, resampling, method="grid", control, measures, aggr, model=F) {
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
	
	if (missing(control)) {
		stop("You have to pass a control object!")
	}
	if (method != control["method"]) 
		stop(paste("Method is '", method, "'. You have passed a control object of the wrong type: ", control["method"], sep=""))
	
	if (control["tune.threshold"] && task["class.nr"] != 2) 
		stop("You can only tune the threshold for binary classification!")
	
	
	
	assign(".mlr.tuneeval", 0, envir=.GlobalEnv)
	
	#.mlr.local$n.eval <<- 0
	#export.tune(learner, task, loss, scale)
	
	
	or = optim.func(learner=learner, task=task, resampling=resampling, control=control, measures=measures, aggr=aggr)

	
	or@opt$par = scale.par(or@opt$par, control)
	if (model) {
		or@model = train(learner, task, parset=or["par"]) 	
	}
	
	return(or)			
}


scale.par <- function(p, control) {
	sc = control["scale"]
	if (identical(sc, identity))
		y = as.list(p)
	else
		y = as.list(sc(unlist(p)))
	names(y) = control["parnames"]
	return(y)
}

