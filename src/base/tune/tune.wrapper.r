
#' Fuses a base learner with a search strategy to select its hyperparameters. Creates a learner object, which can be
#' used like any other learner object, but which internally uses tune. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set of hyperparameter values. Finally, a model is fitted on the complete training data with these optimal
#' hyperparameters and returned.    
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'   Resampling strategy to evaluate points in hyperparameter space.
#' @param control [\code{\linkS4class{tune.control}}] \cr
#'   Control object for search method. Also selects the optimization algorithm for tuning.   
#' @param measure [\code{\linkS4class{measure}}]\cr
#'   Performance measure to optimize. 
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @export
#'
#' @seealso \code{\link{tune}}, \code{\link{grid.control}}, \code{\link{optim.control}}, \code{\link{cmaes.control}} 
#'   
#' @title Fuse learner with tuning.

make.tune.wrapper <- function(learner, resampling, control, measure) {
	make.opt.wrapper(learner, resampling, control, measure)
}

