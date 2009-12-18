#' @include task.learn.r
roxygen()


#' Optimizes the hyperparameters of a learner for a classification or regression problem.
#' Allows for different optimization methods, commonly grid search is used but other search techniques
#' are available as well.  
#' Given some ranges for one or more hyperparameters, it estimates the performance
#' of the learner for each possible combination of the proposed values by
#' using a resampling method (e.g. cross-validation) and returns the best parameter set and its
#' performance.
#'
#' @param learner [\code{\linkS4class{wrapped.learner}}] \cr
#'    Learning method.
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'    Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'    Resampling strategy to evaluate points in hyperparameter space.
#' @param fixed [\code{\link{list}}] \cr
#'    Named list of hyperparameter values which are kept fixed during the optimization. Default is list().   
#' @param method [\code{\link{character}}] \cr
#'    Search method. Currently supported are "grid", "pattern", "cmaes".   
#' @param control \cr
#'    Control object for search method.   
#' @param loss [\code{\linkS4class{loss}}] or [\code{\link{character}}]\cr
#'    Loss to use for tuning. Default is "zero-one" for classification and "squared" error for regression.
#' @param model [\code{\link{logical}}]\cr
#'    Should a final model be fitted on the complete data with the best found hyperparameters?
#' @param scale [\code{\link{function}}]
#'    A function to scale the hyperparamters. E.g. maybe you want to optimize in some log-space.
#'    Has to take a single, numerical vector and return a scaled one. Default is identity function.
#' 
#' @return A list. Might contain some additional information from the optimizer and at least:
#'   \item{par}{Named list of best found hyperparamters.}
#'   \item{perf}{Best found performance value.}
#'   \item{model}{Fitted model on complete data set - if requested.}
#' 
#' @export
#'
#' @usage tune(learner, task, resampling, fixed=list(), method="grid", control=NULL, loss, model=F, scale=I)
#'
#' @examples
#' ct <- make.classif.task(data=iris, target="Species")
#' r <- list(C=2^(-1:1), sigma=2^(-1:1))
#' res <- make.cv.instance(size=nrow(iris), iters=3)
#' tune("kernlab.svm.classif", ct, res, control=grid.control(ranges=r))
#'  
#' @title Hyperparameter tuning


tune <- function(learner, task, resampling, fixed=list(), method="grid", control=NULL, loss, model=F, scale=I) {	
	if (missing(loss))
		loss = default.loss(task)
	if (is.character(loss))
		loss <- make.loss(loss)
	
	
	if (method == "grid")
		optim.func <- tune.grid
	
	if (method == "pattern")
		optim.func <- tune.ps
	
	if(method == "cmaes")
		optim.func <- tune.cmaes
	
	export.tune(learner, task, fixed, loss, scale)
	or <- optim.func(learner, task, resampling, loss, control)
	or$par = scale(or$par)
	if (model) {
		parset = c(fixed, or$par)
		or$model = train(learner, task, parset=parset) 	
	}
	
	return(or)			
}