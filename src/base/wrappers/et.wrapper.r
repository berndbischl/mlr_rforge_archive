#' @include base.wrapper.r


#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on it, the preprocessing is always invoked before.
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param id [string] \cr
#'        Id for resulting learner object. If missing, id of "learner" argument is used.
#' @param label [string] \cr
#'        Label for resulting learner object. If missing, label of "learner" argument is used.
#' @param fun [function] \cr
#'        Function to preprocess a data.frame. First argument must be called 'data', which will be preprocessed and subsequently returned.
#' @param ... [any] \cr
#'        Optional parameters to control the preprocessing. Passed to fun.   
#' 
#' @return \code{\linkS4class{learner}}.
#' 
#' @title Fuse learner with preprocessing.
#' @export

make.et.wrapper = function(learner, id=as.character(NA), label=as.character(NA), measures, aggr, task, minimze=TRUE, thresholds=50) {
	if (is.character(learner))
		learner = make.learner(learner)
	fun = function(pred, measures=measures, aggr=aggr, task=task, minimize=minimize, thresholds=thresholds) {
		tt = tune.threshold(pred, measures, aggr, task, minimize=minimize, thresholds=thresholds) 
	}	
	make.postproc.wrapper(learner, id=id, label=label, fun=fun)
}





