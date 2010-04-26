
#' Fuses a base learner with a search strategy to select its hyperparameters. Creates a wrapped.learner object, which can be
#' used like any other learner object, but which internally uses tune. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set of hyperparameter values. Finally, a model is fitted on the complete training data with these optimal
#' hyperparameters and returned.    
#'
#' @param learner [\code{\linkS4class{wrapped.learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param id [string]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param label [string]\cr 
#'        Label string for object. Used in plots, etc.  
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'        Resampling strategy to evaluate points in hyperparameter space.
#' @param method [\code{\link{character}}] \cr
#'        Search method. Currently supported are grid search "grid", pattern search "pattern", CMA-ES "cmaes" and Nelder-Mead "nm".   
#' @param control 
#'        Control object for search method.   
#' @param measures [see \code{\link{measures}}]
#'        Performance measures. 
#' @param aggr [see \code{\link{aggregations}}]
#'        Aggregation functions. 
#' 
#' @return \code{\linkS4class{wrapped.learner}}.
#' 
#' @export
#'
#' @usage make.tune.wrapper(learner, id, label, resampling, method="grid", control, measures, aggr)
#'
#' @seealso \code{\link{tune}}, \code{\link{grid.control}}, \code{\link{ps.control}}, \code{\link{cmaes.control}}, \code{\link{nm.control}}
#'   
#' @title Fuse learner with tuning.

make.tune.wrapper <- function(learner, id, label, resampling, method="grid", control, measures, aggr) {
	if (is.character(learner))
		learner = make.learner(learner)
	if (missing(id))
		id = learner["id"]
	if (missing(label))
		label = id
	if (missing(measures))
		measures = default.measures(learner)
	measures = make.measures(measures)
	if (missing(aggr))
		aggr = default.aggr()
	aggr = make.aggrs(aggr)
	if (is(learner, "wrapped.learner.classif"))
		tt = new("opt.wrapper.classif", type="tune", id=id, label=label, base.learner=learner, resampling=resampling, 
				method=method, control=control, measures=measures, aggr=aggr)
	else		
		tt = new("opt.wrapper.regr", type="tune", id=id, label=label, base.learner=learner, resampling=resampling, 
				method=method, control=control, measures=measures, aggr=aggr)
	return(tt)
}

