#' Fuse learner with filter method.
#' 
#' Fuses a base learner with a filter method. Creates a learner object, which can be
#' used like any other learner object. 
#' Internally Uses \code{\link[mlrFeatSel]{filterFeatures}} before every model fit. 
#' 
#' Look at package FSelector for details on the filter algorithms. 
#' 
#' @param learner [\code{\link[mlr]{Learner}}]\cr 
#'   The learner.  
#' @param fw.method [\code{character(1)}]\cr
#'   Filter method. Available are:
#'   linear.correlation, rank.correlation, information.gain, gain.ratio, symmetrical.uncertainty, chi.squared, random.forest.importance, relief, oneR
#'   Default is random.forest.importance.
#' @param fw.perc [\code{numeric(1)}]\cr
#'   Percentage of highest ranking features to select after filtering.  
#'   Default is 1 (=100 percent).
#' @return [\code{\link{Learner}}].
#' @export
makeFilterWrapper = function(learner, fw.method="random.forest.importance", fw.perc=1) {
  
  meths = c("linear.correlation", "rank.correlation", "information.gain", "gain.ratio", 
    "symmetrical.uncertainty", "chi.squared", "random.forest.importance", "relief", "oneR")
  checkArg(fw.method, choices=meths)
  checkArg(fw.perc, "numeric", len=1L, na.ok=FALSE, lower=0, upper=1)
  ps = makeParamSet(
    makeDiscreteLearnerParam(id="fw.method", values=meths),
    makeNumericLearnerParam(id="fw.perc")
  )
  pv = list(fw.method=fw.method, fw.perc=fw.perc)
  # fixme scale to 0,1
  makeBaseWrapper(learner, package="FSelector", par.set=ps, par.vals=pv, 
    cl="FilterWrapper")
  # fixme: check that for some the inputs have to be all num. or accept error in train and NA in predict?
}

#' @S3method trainLearner FilterWrapper
trainLearner.FilterWrapper = function(.learner, .task, .subset, fw.method, fw.perc, ...) {
  print("train: filter")

  .task = subsetTask(.task, subset=.subset)  
  tn = .task$task.desc$target
  vals = filterFeatures(.task)
  #vals = sort(vals, decreasing=TRUE)
  inds = seq_len(round(fw.perc*length(vals)))
  features = names(vals)[inds]
  if (length(features) > 0) {
    .task = subsetTask(.task, features=features)  
    # !we have already subsetted!
    m = mlr:::trainLearner(.learner$learner, .task, 1:.task$task.desc$size, ...)
  } else {
    # !we have already subsetted!
    m = mlr:::makeNoFeaturesModel(targets=getTaskTargets(.task), task.desc=.task$task.desc)
  }
  # set the features as attribute, so we can extract it later 
  attr(m, "filter.result") = features
  return(m)
}

#' @S3method predictLearner FilterWrapper
predictLearner.FilterWrapper = function(.learner, .model, .newdata, ...) {
  print("predict: filter")
  .newdata = .newdata[, .model$features, drop=FALSE]  
  predictLearner(.learner$learner, .model, .newdata, ...)
}

#' @S3method makeWrappedModel FilterWrapper
makeWrappedModel.FilterWrapper = function(learner, model, task.desc, subset, features, time) {
  x = NextMethod()
  class(x) = c("FilterModel", class(x))
  x$features = attr(model, "filter.result")
  attr(x$model, "filter.result") = NULL
  return(x)
}




