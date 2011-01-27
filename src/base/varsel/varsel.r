#' Optimizes the variables for a classification or regression problem by choosing a variable selection wrapper approach.
#' Allows for different optimization methods, such as forward search or a genetic algorithm.
#' You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at the 
#' subclasses of [\code{\linkS4class{varsel.control}}].
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'   Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'   Resampling strategy to evaluate feature sets. If you pass a description, 
#'   it is instantiated once at the beginning by default, so all feature sets are evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at the control object.  
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param control [see \code{\link{varsel.control}}]
#'   Control object for search method. Also selects the optimization algorithm for feature selection. 
#' @param measures [list of \code{\linkS4class{measure}}]\cr
#'   Performance measures to evaluate. The first measure, aggregated by the first aggregation function is optimized during tuning, others are simply evaluated.  
#' 
#' @return \code{\linkS4class{opt.result}}.
#' 
#' @export
#' @seealso \code{\link{make.varsel.wrapper}} 
#' @title Variable selection.

varsel <- function(learner, task, resampling, control, measures) {
  if (is.character(learner))
    learner <- make.learner(learner)
  if (is(resampling, "resample.desc") && control@same.resampling.instance)
    resampling = make.resample.instance(resampling, task=task)
  if (missing(measures))
    measures = default.measures(task)
  if (is(measures, "measure"))
    measures = list(measures)   
  
	cl = as.character(class(control))
	
	sel.func = switch(cl,
			sequential.control = varsel.seq,
			randomvarsel.control = varsel.random,
      exhvarsel.control = varsel.exhaustive,
      stop(paste("Feature selection algorithm for", cl, "does not exist!"))
	)

	if (missing(control)) {
		stop("You have to pass a control object!")
	}
  opt.path = makeOptimizationPathFromMeasures(par.set, measures)
  or = sel.func(learner, task, resampling, measures, par.set, control, opt.path)
	if (model) {
    task = subset(task, vars=or["par"])
		or@model = train(learner, task) 	
	}
	return(or)
}
