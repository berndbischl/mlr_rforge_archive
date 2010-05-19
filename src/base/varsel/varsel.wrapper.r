
#' Fuses a base learner with a search strategy to select variables. Creates a wrapped.learner object, which can be
#' used like any other learner object, but which internally uses varsel. If the train function is called on it, the search strategy and resampling are invoked
#' to select an optimal set variables. Finally, a model is fitted on the complete training data with these variables and returned.    
#'
#' @param learner [\code{\linkS4class{wrapped.learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param id [string]\cr 
#'        Id string for object. Used to select the object from a named list, etc.  
#' @param label [string]\cr 
#'        Label string for object. Used in plots, etc.  
#' @param resampling [\code{\linkS4class{resample.instance}}] or [\code{\linkS4class{resample.desc}}]\cr
#'        Resampling strategy to evaluate points in hyperparameter space.
#' @param type [string] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to predict.
#'        Default is "response". Use "prob" if you want to tune the threshold. "decision" is not supported at the moment.
#' 		  Ignored for regression.	 
#' @param method [\code{\link{character}}] \cr
#'        Search method. Currently supported are sequential forward search "sfs", sequential backward search "sbs", 
#'        sequential floating forward search "sffs", sequential floating backward search "sfbs" and a monte-carlo search 
#'        "random".    
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
#' @usage make.tune.wrapper(learner, id, label, resampling, type="response", method="sfs", control, measures, aggr)
#'
#' @seealso \code{\link{varsel}}, \code{\link{varsel.control}} 
#'   
#' @title Fuse learner with variable selection.

make.varsel.wrapper <- function(learner, id, label, resampling, measures, aggr, method="sfs", control) {
	make.opt.wrapper("varsel", learner, id, label, resampling, type=type, method=method, control, measures, aggr)
}

