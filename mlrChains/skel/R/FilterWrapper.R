#' Fuse learner with filter method.
#' 
#' Fuses a base learner with a filter method. Creates a learner object, which can be
#' used like any other learner object. 
#' Internally Uses \code{\link{varfilter}} before every model fit. 
#' 
#' Look at package FSelector for details on the filter algorithms. 
#' 
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param fw.method [\code{character(1)}] \cr
#'   Filter method. Available are:
#'   linear.correlation, rank.correlation, information.gain, gain.ratio, symmetrical.uncertainty, chi.squared, random.forest.importance, relief, oneR
#' @param fw.threshold [single double] \cr
#'   Only features whose importance value exceed this are selected.  
#' @return \code{\link{Learner}}.
#' @export
makeFilterWrapper = function(learner, fw.method="information.gain", fw.threshold=1) {
  if (is.character(learner))
    learner = makeLearner(learner)
  # fixme: check that for some the inputs have to be all num. or accept error in train and NA in predict?
  ps = makeParamSet(
    makeDiscreteLearnerParam(id="fw.method",
      values=c("linear.correlation", "rank.correlation", "information.gain", "gain.ratio", 
        "symmetrical.uncertainty", "chi.squared", "random.forest.importance", "relief", "oneR")),
    makeNumericLearnerParam(id="fw.threshold")
  )
	w = new("FilterWrapper", learner=learner, pack="FSelector", par.set=ps, 
    par.vals=list(fw.method=fw.method, fw.threshold=fw.threshold))
  setPredictType(w, learner$predict.type)
}

#' @S3method trainLearner FilterWrapper
trainLearner = function(.learner, .task, .subset,  ...) {
  pvs = .learner$par.vals 
  .task = subsetTask(.task, subset=.subset)  
  tn = .task$desc$target
  vars = varfilter(.task, pvs$fw.method, pvs$fw.threshold)$vars
  if (length(vars) > 0) {
    .task = subsetData(.task, vars=vars)  
    # !we have already subsetted!
    m = trainLearner(.learner$learner, .task, 1:.task$task.desc$size, ...)
  } else {
    # !we have already subsetted!
    m = new("novars", targets=getTargets(.task), desc=.task$desc)
  }
  # set the vars as attribute, so we can extract it later 
  attr(m, "filter.result") = vars
  return(m)
}

#' @S3method predictLearner FilterWrapper
predictLearner = function(.learner, .model, .newdata, .type, ...) {
  .newdata = .newdata[, .model$vars, drop=FALSE]  
  predictLearner(.learner$learner, .model, .newdata, .type, ...)
}

#' @S3method makeWrappedModel FilterWrapper
makeWrappedModel.FilterWrapper = function(learner, model, task.desc, subset, vars, time) {
  x = makeWrappedModel(learner, model, task.desc, subset, vars, time)  
  vars = attr(model, "filter.result")
  attr(model, "filter.result") = NULL
}




