#' @include wrapped.learner.r
#' @include wrapped.learner.classif.r
#' @include wrapped.learner.regr.r
#' @include resample.instance.r
#' @include train.learner.r
#' @include predict.learner.r

setClass(
		"tune.wrapper",
		contains = c("wrapped.learner"),
		representation = representation(
				base.learner = "wrapped.learner",
				method = "character",
				resampling = "resample.desc",
				control = "ANY",
				measures = "list",
				aggr = "list"
		)
)

setClass(
		"tune.wrapper.classif",
		contains = c("tune.wrapper", "wrapped.learner.classif")
)

setClass(
		"tune.wrapper.regr",
		contains = c("tune.wrapper", "wrapped.learner.regr")
)


#' Constructor.
#' @title Constructor for tuning wrapper
setMethod(
		f = "initialize",
		signature = signature("tune.wrapper"),
		def = function(.Object, base.learner, id, label, resampling, method, control) {
			if (missing(base.learner))
				return(.Object)
			bl = base.learner
			.Object@base.learner = bl
			.Object@method = method
			.Object@resampling = resampling
			.Object@control = control
			callNextMethod(.Object, id=id, label=label, pack="mlr", props=bl@props)
		}
)

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
	if (is(learner, "wrapped.learner.classif"))
		tt = new("tune.wrapper.classif", id=id, label=label, base.learner=learner, resampling=resampling, 
				method=method, control=control, measures=measures, aggr=aggr)
	else		
		tt = new("tune.wrapper.regr", id=id, label=label, base.learner=learner, resampling=resampling, 
				method=method, control=control, measures=measures, aggr=aggr)
	return(tt)
}

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="tune.wrapper", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			wl = .wrapped.learner
			bl = wl@base.learner
			if (is(bl, "wrapped.learner.classif"))
				f = make.task
			else if (is(bl, "wrapped.learner.regr"))
				f = make.task
			
			lt = f(data=.data, target=.targetvar)	
			tr = tune(bl, task=lt, resampling=wl@resampling, method=wl@method, control=wl@control, 
					measures=wl@measures, aggr=wl@aggr, model=TRUE)
			m = tr@model["learner.model"]
			attr(m, "opt") = tr@opt
			attr(m, "path") = tr@path
			return(m)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "tune.wrapper", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			predict.learner(.wrapped.learner@base.learner, .wrapped.model, .newdata, .type, ...)
		}
)	

