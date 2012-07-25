#fixme use learnerparam or ordinary params?

#' Fuse learner with preprocessing.
#'
#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on it, the preprocessing is always invoked before.
#'
#' @param learner [\code{\link[mlr]{Learner}}]\cr 
#'   The learner.
#' @param train [\code{function(data, target, args}]\cr
#'   Function to preprocess the data before training. 
#'   \code{target} is a string and denotest the target variable in \code{data}.
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
#'   Named list of default values for params in \code{args} repectively \code{par.set}.
#'   Default is empty list.
#' @return [\code{\link[mlr]{Learner}}].
#' @export
makePreprocWrapper = function(learner, train, predict, par.set=makeParamSet(), par.vals=list()) {
  checkArg(train, formals=c("data", "target", "args"))
  checkArg(predict, formals=c("data", "target", "args", "control"))
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
  p = .learner$train(data=d, target=tn, args=pvs)
  if (!(is.list(p) && length(p)==2 && all(names(p) == c("data", "control")) 
        && is.data.frame(p$data) && is.list(p$control)))
    stop("Preprocessing train must result in list wil elements data[data.frame] and control[list]!")
  .task = mlr:::changeData(.task, p$data)
  # we have already subsetted!
  # fixme: do.call
  args = list(.learner$learner, .task, 1:.task$task.desc$size)
  args2 = list(...)
  # remove preproc pars
  args2 = args2[setdiff(names(args2), names(pvs))]
  m = do.call(trainLearner, c(args, args2))
  structure(list(
    prev.model = m,
    control = p$control
  ), class = "mlrChains.PreprocModel")
}

#' @S3method predictLearner PreprocWrapper
predictLearner.PreprocWrapper = function(.learner, .model, .newdata, ...) {
  print("predict: preproc")
  print(class(.model))
  print(class(.model$learner.model))
  pvs = .model$learner$par.vals
  m = .model$learner.model
  .newdata = .learner$predict(.newdata, .model$task.desc$target, pvs, m$control)
  print(str(.newdata))
  xx <<- m
  if (!is.data.frame(.newdata))
    stop("Preprocessing must result in a data.frame!")
  print(names(m))
  print(str(m$prev.model))
  predictLearner(.learner$learner, m$prev.model, .newdata, ...)
  stop(89)
}

