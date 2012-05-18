#' Predict the target variable of new data using a fitted model. If the type is set to "prob"
#' probabilities will be stored in the resulting object. The resulting class labels are 
#' the classes with the maximum values. 
#' 
#' @param object [\code{\link{WrappedModel}}]\cr 
#'   Wrapped model, trained from a learn task.  
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   Specifies learning task. If this is passed, data from this task is predicted.   
#' @param subset [\code{integer}]\cr 
#'   Index vector to subset the data in the task to use for prediction. 
#' @param newdata [\code{\link{data.frame}}]\cr 
#'   New observations which should be predicted. Alternatively pass this instead of task. 
#' @return \code{\link{Prediction}}.
#'
#' @export
#' @rdname predict
#' @importFrom stats predict
#' @seealso \code{\link{train}}, \code{\link{setThreshold}}
#' @title Predict new data.

predict.WrappedModel = function(object, task, newdata, subset) {
  if (!missing(task) && !missing(newdata)) 
    stop("Pass either a task object or a newdata data.frame to predict, but not both!")
  model = object
  wl = model$learner
  td = model$task.desc
  
  if (missing(newdata)) {
    if (missing(subset))
      subset = 1:task$desc$size
    newdata = getTaskData(task, subset=subset)
  } else {
    if (!is.data.frame(newdata) || nrow(newdata) == 0)
      stop("newdata must be a data.frame with at least one row!")
  }
  # load pack. if we saved a model and loaded it later just for prediction this is necessary
  requirePackages(wl$pack, paste("learner", learner$id))
  
  cns = colnames(newdata)
  tn = td$target
  t.col = which(cns == tn)
  # get truth and drop target col, if target in newdata
  if (length(t.col) == 1) {
    truth = newdata[, t.col]
    newdata = newdata[, -t.col, drop=FALSE]					
    
  } else {
    truth = NULL
  }
  
  if (wl$type == "classif") {
    levs = td$class.levels
  }
  
  response = NULL
  prob = NULL
  time.predict = as.numeric(NA)
  
  # was there an error in building the model? --> return NAs
  if(is(model$learner.model, "FailureModel")) {
    p = predict_nas(model, newdata)
    time.predict = as.numeric(NA)
  } else {
    pars <- list(
        .learner = wl,
        .model = model, 
        .newdata = newdata
    )
    # only pass train hyper pars as basic rlearner in ...
    pars = c(pars, getHyperPars(getLeafLearner(wl), "predict"))
    if(!is.null(.mlr.conf$debug.seed)) {
      set.seed(.mlr.conf$debug.seed)
      warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
    }
    # todo: capture outout, see learner sda
    if(is(model$learner.model, "novars")) {
      p = predict_novars(model, newdata)
      time.predict = 0
    } else {
      if (.mlr.conf$logger.setup$show.learner.output)
        fun1 = identity
      else
        fun1 = capture.output
      if (.mlr.conf$errorhandler.setup$on.learner.error == "stop")
        fun2 = identity
      else
        fun2 = function(x) try(x, silent=TRUE)
      st = system.time(or <- fun1(p <- fun2(do.call(predictLearner, pars))), gcFirst = FALSE)
      time.predict = as.numeric(st[3])
      # was there an error during prediction?
      if(is(p, "try-error")) {
        msg = as.character(p)
        if (.mlr.conf$errorhandler.setup$on.learner.error == "warn")
          warning("Could not predict the learner: ", msg)
        p = predict_nas(model, newdata)
        time.predict = as.numeric(NA)
      }
    }
    if (wl$type == "classif") {
      if (wl$predict.type == "response") {
        # the levels of the predicted classes might not be complete....
        # be sure to add the levels at the end, otherwise data gets changed!!!
        if (!is.factor(p))
          stop("predictLearner for ", class(wl), " has returned a class ", class(p), " instead of a factor!")
        levs2 = levels(p)
        if (length(levs2) != length(levs) || any(levs != levs2))
          p = factor(p, levels=levs)
        
      } else if (wl$predict.type == "prob") {
        if (!is.matrix(p))
          stop("predictLearner for ", class(wl), " has returned a class ", class(p), " instead of a matrix!")
        cns = colnames(p)
        if (is.null(cns) || length(cns) == 0)
          stop("predictLearner for ", class(wl), " has returned not the class levels as column names, but no column names at all!")
        if (!setequal(cns, levs))
          stop("predictLearner for ", class(wl), " has returned not the class levels as column names:", colnames(p))
        
      } else if (wl$predict.type == "se") {
        if (!is.matrix(p))
          stop("predictLearner for ", class(wl), " has returned a class ", class(p), " instead of a matrix!")
        if (ncol(p)!= 2)
          stop("predictLearner for ", class(wl), " has not returned a numeric matrix with 2 columns!")
      }
    } else if (is(model, "WrappedModel.Regr")) {
      if (class(p) != "numeric")
        stop("predictLearner for ", class(wl), " has returned a class ", class(p), " instead of a numeric!")
    }
  }
  if (missing(task))
    ids = NULL			
  else
    ids = subset
  makePrediction(task.desc=td, id=ids, truth=truth, 
      predict.type=wl$predict.type, y=p, time=time.predict)
}

predict_nas = function(model, newdata) {
	if (model$learner$type == "classif") {
    levs = model$task.desc$class.levels  
		p = switch(model$learner$predict.type, 
				response = factor(rep(NA, nrow(newdata)), levels=levs),
				matrix(as.numeric(NA), nrow=nrow(newdata), ncol=length(levs), dimnames=list(NULL, levs))
		)
	} else {
		p = as.numeric(rep(NA, nrow(newdata)))
	}
	return(p)
}

predict_novars = function(model, newdata) {
  y = model$learner.model$targets
  # for regression return constant mean
  if (model$learner$type == "regr")
    return(rep(mean(y), nrow(newdata)))
  tab = prop.table(table(y))
  probs = as.numeric(tab) 
  if(model$learner$predict.type == "response")
    return(sample(as.factor(names(tab)), nrow(newdata), prob=probs, replace=TRUE))  
  else {
    probs = t(replicate(nrow(newdata), probs))
    colnames(probs) = names(tab)
    return(probs)
  }
}



