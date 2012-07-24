#fixme use learnerparam or ordinary params?

#' Fuse learner with preprocessing.
#'
#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on it, the preprocessing is always invoked before.
#'
#' @param learner [\code{\link[mlr]{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param train [\code{function(data, target, args}]\cr
#'   Function to preprocess the data before training. 
#'   \code{target} is a string and denotest the target variable in \cod€{data}.
#'   \code{args} is a list of further arguments and parameters to influence the 
#'   preprocessing.
#'   Must return a \code{list(data, control)}, where \code{data} is the preprocessed 
#'   data and \code{control} stores all information necessary to do the preprocessing
#'   before predictions.
#' @param predict [\code{function(data, target, args, control}]\cr
#'   Function to preprocess the data before prediction. 
#'   \code{target} is a string and denotest the target variable in \code{data}.
#'   \code{args} are the args that were passed to \code{train}.
#'   \code{control} is the object you returned in \code{train}.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Parameter set of \code{\link[ParamHelpers]{LearnerParam}} objects to describe the 
#'   parameters in \code{args}.
#'   Default is empty set.
#' @param par.vals [\code{list}]\cr
#'   Named list of default values for params in \code{args} / \code{par.set}.
#'   Default is empty list.
#' @return \code{\link[mlr]{Learner}}.
#' @export
makePreprocWrapper = function(learner, train, predict, par.set=makeParamSet(), par.vals=list()) {
  checkArg(train, formals=c("data", "targetvar", "args"))
  checkArg(predict, formals=c("data", "targetvar", "args", "control"))
  x = makeBaseWrapper(learner=learner, par.set=par.set, par.vals=par.vals, cl="PreprocWrapper")
  x$train = train
  x$predict = predict
  return(x)
}

#' @S3method trainLearner PreprocWrapper
trainLearner.PreprocWrapper = function(.learner, .task, .subset,  ...) {
  pvs = .learner$par.vals
  d = getTaskData(.task, .subset)
  tn = .task$task.desc$target
  p = .learner$train(data=d, targetvar=tn, args=pvs)
  if (!(is.list(p) && length(p)==2 && all(names(p) == c("data", "control")) 
        && is.data.frame(p$data) && is.list(p$control)))
    stop("Preprocessing train must result in list wil elements data[data.frame] and control[list]!")
  .task = mlr:::changeData(.task, p$data)
  # we have already subsetted!
  m = trainLearner(.learner$learner, .task, 1:.task$task.desc$size, ...)
  attr(m, "control") = p$control
  return(m)
}

#' @S3method predictLearner PreprocWrapper
predictLearner.PreprocWrapper = function(.learner, .model, .newdata, ...) {
  pvs = .model$learner$par.vals
  m = nrow(.newdata)
  .newdata = .learner$predict(.newdata, .model$task.desc$target, pvs, .model$control)
  if (!is.data.frame(.newdata))
    stop("Preprocessing must result in a data.frame!")
  predictLearner(.learner$learner, .model, .newdata, ...)
}

#' @S3method makeWrappedModel PreprocWrapper
makeWrappedModel.PreprocWrapper = function(learner, model, task.desc, subset, features, time) {
  x = NextMethod()
  x$control = attr(model, "control")
  attr(x$model, "control") = NULL
  class(x) = c("PreprocModel", class(x))  
  return(x)
}
