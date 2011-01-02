
#' Optimizes the variables for a classification or regression problem by choosing a variable selection wrapper approach.
#' Allows for different optimization methods.
#' The specific details of the search algorithm are set by passing a control object.
#' Currently you can use \code{\link{sequential.control}} and \code{\link{randomvarsel.control}}. 
#' The first measure, aggregated by the first aggregation function is optimized, to find a set of optimal variables.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.   
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'        Resampling strategy to evaluate points in hyperparameter space.
#' @param control [see \code{\link{varsel.control}}]
#'        Control object for search method. Also selects the optimization algorithm for feature selection. 
#' @param measures [see \code{\link{measures}}]\cr
#'        Performance measures. 
#' @param model [boolean]\cr
#'        Should a final model be fitted on the complete data with the best found features? Default is FALSE.
#' 
#' @return \code{\linkS4class{opt.result}}.
#' 
#' @export
#'
#' @seealso \code{\link{varsel.control}}, \code{\link{make.varsel.wrapper}} 
#'   
#' @title Variable selection.

varsel <- function(learner, task, resampling, control, measures, model=FALSE) {
  if (is.character(learner))
    learner <- make.learner(learner)
  # todo: should we really do this here??
  # convert to instance so all pars are evaluated on the same splits
  if (is(resampling, "resample.desc")) 
    resampling = make.res.instance(resampling, task=task)
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
	
	assign(".mlr.vareval", 0, envir=.GlobalEnv)
	
	or = sel.func(learner=learner, task=task, resampling=resampling, 
			measures=measures, control=control) 
	if (model) {
		or@model = train(learner, task, vars=or["par"]) 	
	}
	return(or)
}
