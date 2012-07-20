#' Predict new data.
#' 
#' Predict the target variable of new data using a fitted model. 
#' What is stored exactly in the [\code{\link{Prediction}}] object depends
#' on the \code{predict.type} setting of the \code{\link{Learner}}.
#' 
#' @param object [\code{\link{WrappedModel}}]\cr 
#'   Wrapped model, result of \code{\link{train}}.
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task. If this is passed, data from this task is predicted.   
#' @param newdata [\code{data.frame}]\cr 
#'   New observations which should be predicted. 
#'   Pass this alternatively instead of \code{task}. 
#' @param subset [\code{integer}]\cr 
#'   Index vector to subset \code{task} or \code{newdata}.
#'   Default is all data.
#' @param ... \cr 
#'   Currently ignored.
#' @return [\code{\link{Prediction}}].
#' @method predict WrappedModel
#' @S3method predict WrappedModel
predict.WrappedModel = function(object, task, newdata, subset, ...) {
  if (!missing(task) && !missing(newdata)) 
    stop("Pass either a task object or a newdata data.frame to predict, but not both!")
  checkArg(object, "WrappedModel")
  model = object
  learner = model$learner
  td = model$task.desc
  
  #size = function(x) {
  #  if (i)
  #}
 
  if (missing(newdata)) {
    checkArg(task, "SupervisedTask")
    newdata = getTaskData(task, subset)
  } else {
    checkArg(newdata, "data.frame")
    # FIXME check that data is of same structure?
    if (nrow(newdata) == 0)
      stop("newdata must be a data.frame with at least one row!")
  }
  if (missing(subset)) {
    subset = 1:nrow(newdata)
  } else {
    subset = convertIntegers(subset)
    checkArg(subset, "integer", na.ok=FALSE)
  }

  # if we saved a model and loaded it later just for prediction this is necessary
  requireLearnerPackages(learner)
  cns = colnames(newdata)
  tn = td$target
  t.col = which(cns == tn)
  # get truth and drop target col, if target in newdata
  if (length(t.col) == 1) {
    #FIXME this copies data
    truth = newdata[, t.col]
    newdata = newdata[, -t.col, drop=FALSE]					
  } else {
    truth = NULL
  }

  response = NULL
  prob = NULL
  time.predict = as.numeric(NA)
  
  # was there an error in building the model? --> return NAs
  if(is(model$learner.model, "FailureModel")) {
    p = predict_nas(model, newdata)
    time.predict = as.numeric(NA)
  } else {
    #FIXME this copies newdata
    pars = list(
      .learner = learner,
      .model = model, 
      .newdata = newdata
    )
    # only pass train hyper pars as basic rlearner in ...
    pars = c(pars, getHyperPars(getLeafLearner(learner), "predict"))
    debug.seed = getOption("mlr.debug.seed", NULL)
    if(!is.null(debug.seed))
      set.seed(debug.seed)
    if(inherits(model$learner.model, "NoFeaturesModel")) {
      p = predict_nofeatures(model, newdata)
      time.predict = 0
    } else {
      if (getOption("mlr.show.learner.output"))
        fun1 = identity
      else
        fun1 = capture.output
      if (getOption("mlr.on.learner.error") == "stop")
        fun2 = identity
      else
        fun2 = function(x) try(x, silent=TRUE)
      st = system.time(fun1(p <- fun2(do.call(predictLearner2, pars))), gcFirst = FALSE)
      # FIXME conversions and checks missing!!
      time.predict = as.numeric(st[3])
      # was there an error during prediction?
      if(is.error(p)) {
        if (getOption("mlr.on.learner.error") == "warn")
          warningf("Could not predict with learner %s: %s", learner$id, as.character(p))
        p = predict_nas(model, newdata)
        time.predict = as.numeric(NA)
      }
    }
  }
  if (missing(task))
    ids = NULL			
  else
    ids = subset
  makePrediction(task.desc=td, id=ids, truth=truth, 
    predict.type=learner$predict.type, y=p, time=time.predict)
}

