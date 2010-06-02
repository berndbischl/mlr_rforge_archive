
#' Optimizes the variables for a classification or regression problem by choosing a variable selection wrapper approach.
#' Allows for different optimization methods.
#' The specific details of the search algorithm are set by passing a control object.   
#' 
#' The first measure, aggregated by the first aggregation function is optimized, to find a set of optimal variables.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'        Resampling strategy to evaluate points in hyperparameter space.
#' @param method [\code{\link{character}}] \cr
#'        Search method. Currently supported are sequential forward search "sfs", sequential backward search "sbs", 
#'        sequential floating forward search "sffs", sequential floating backward search "sfbs" and a monte-carlo search 
#'        "random".    
#' @param control [see \code{\link{varsel.control}}]
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
#' @seealso \code{\link{varsel.control}}, \code{\link{make.varsel.wrapper}} 
#'   
#' @title Variable selection.

varsel <- function(learner, task, resampling, method="sfs", control, measures, aggr, model=F) {
	if (missing(measures))
		measures = default.measures(task)
	measures = make.measures(measures)
	
	if (missing(aggr))
		aggr = default.aggr(task)
	aggr = make.aggrs(aggr)
	
	if (is.character(method)) {
		sel.func = switch(method,
				sfs = varsel.seq,
				sbs = varsel.seq,
				sffs = varsel.seq,
				sfbs = varsel.seq,
				random = varsel.random,
				bestcor = varsel.bestcor,
				hybrid = varsel.hybrid,
				hybrid2 = varsel.hybrid2,
				stop(paste("Method", method, "does not exist!"))
		)
	} else {
		sel.func = method
	}	
	if (missing(control)) {
		stop("You have to pass a control object!")
	}
	if (control["tune.threshold"] && task["class.nr"] != 2) 
		stop("You can only tune the threshold for binary classification!")
	
	assign(".mlr.vareval", 0, envir=.GlobalEnv)
	
	or = sel.func(learner=learner, task=task, resampling=resampling, 
			measures=measures, aggr=aggr, method=method, control=control) 
	if (model) {
		or@model = train(learner, task, vars=or["par"]) 	
	}
	return(or)
}
