#' Does model selection from a finite set of learners.
#' 
#' The first measure, aggregated by the first aggregation function is optimized.
#'
#' @param learners [string | \code{\linkS4class{learner}} | list of the previous two] \cr
#'   	Defines the learning algorithms which should be compared.
#' @param task [\code{\linkS4class{LearnTask}}] \cr
#'   	Learning task.   
#' @param resampling [\code{\linkS4class{ResampleInstance}}] or [\code{\linkS4class{ResampleDesc}}]\cr
#'   	Resampling strategy to evaluate points in hyperparameter space. At least for grid search, if you pass a description, 
#'		it is instantiated at one, so all points are evaluated on the same training/test sets.	
#' @param control [\code{\linkS4class{modelsel.control}}] \cr
#'		Control object for comparison strategy.    
#' @param measures [see \code{\link{measures}}]\cr
#'		Performance measures. 
#' @param aggr [see \code{\link{aggregations}}]\cr
#'		Aggregation functions. 
#' @param model [boolean]\cr
#'		Should a final model be fitted on the complete data with the best found learner? Default is FALSE.
#' @param path [boolean]\cr
#'		Should optimization path be saved? Default is FALSE.
#' 
#' @return \code{\linkS4class{opt.result}}.
#' 
#' @export
#'
#' @seealso \code{\link{grid.control}}, \code{\link{optim.control}}, \code{\link{cmaes.control}}

modelsel = function(learners, task, resampling, control, measures, aggr, model=FALSE, path=FALSE) {
  
  be = bench.exp(learners=learners, tasks=task, measures=measures, aggr=aggr)
  m = be["measures"][[1]]
  a = be["aggrs"][[1]]
  p = be["perf", measure=m, aggr=a]
  j = which.min(p)
  best = colnames(p)[j]
}

