#' Fuses a base learner with a preprocessing method. Creates a learner object, which can be
#' used like any other learner object, but which internally preprocesses the data as requested. 
#' If the train or predict function is called on it, the preprocessing is always invoked before.
#'
#' @param learner [\code{\linkS4class{Learner}} or string]\cr 
#'   Learning algorithm. See \code{\link{learners}}.  
#' @param fun [function] \cr
#'   Function to preprocess a data.frame. First argument must be called 'data', which will be preprocessed and subsequently returned.
#' @param ... [any] \cr
#'   Optional parameters to control the preprocessing. Passed to fun.   
#' 
#' @return \code{\linkS4class{Learner}}.
#' 
#' @title Fuse learner with preprocessing.
#' @export

makePreprocWrapper = function(learner, train, predict, par.set=makeParamSet(), par.vals=list()) {
  if (missing(par.set))
    par.set=makeParamSet()
  if (any(names(formals(train)) != c("data", "targetvar", "args")))
    stop("Arguments in preproc train function have to be: data, targetvar, args")		
  if (any(names(formals(predict)) != c("data", "targetvar", "args", "control")))
    stop("Arguments in preproc predict function have to be: data, targetvar, args, control")    
  x = makeBaseWrapper(learner=learner, par.set=par.set, par.vals=par.vals)
  x$train = train
  x$predict = predict
}

#' @S3method trainLearner PreprocWrapper
trainLearner.PreprocWrapper = function(.learner, .task, .subset,  ...) {
  pvs = .learner$par.vals
  d = getData(.task, .subset)
  tn = .task$task.desc$target
  p = .learner$train(data=d, targetvar=tn, args=pvs)
  if (!(is.list(p) && length(p)==2 && all(names(p) == c("data", "control")) 
        && is.data.frame(p$data) && is.list(p$control)))
    stop("Preprocessing train must result in list wil elements data[data.frame] and control[list]!")
  #if (nrow(p$data) != length(.subset))
  #  stop("Preprocessing train may not change number of cases!")
  .task = mlr:::changeData(.task, p$data)
  # we have already subsetted!
  m = trainLearner(.learner$learner, .task, 1:.task$task.desc$size, ...)
  attr(m, "control") = p$control
  return(m)
}

#' @S3method predictLearne PreprocWrapper
predictLearner.PreprocWrapper = function(.learner, .model, .newdata, ...) {
  pvs = .model$learner$par.vals
  m = nrow(.newdata)
  .newdata = .learner$predict(.newdata, .model$task.desc$target, pvs, .model$control)
  if (!is.data.frame( .newdata))
    stop("Preprocessing must result in a data.frame!")
  if (nrow(.newdata) != m)
    stop("Preprocessing predict may not change number of cases!")
  predictLearner(.learner$learner, .model, .newdata, ...)
}

#' @S3method makeWrappedModel PreprocWrapper
makeWrappedModel.PreprocWrapper = function(learner, model, task.desc, subset, vars, time, control) {
  x = makeWrappedModel(learner, model, task.desc, subset, vars, time)
  x$control = control
  class(x) = c("PreprocModel", class(x))  
}
