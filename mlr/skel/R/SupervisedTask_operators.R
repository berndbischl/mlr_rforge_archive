# we create a new env, so the reference is not changed
changeData = function(task, data) {
  task$env = new.env()
  task$env$data = data
  td = task$task.desc
  task$task.desc = makeTaskDesc(td$type, td$id, data, td$target, td$has.blocking, td$positive)
  return(task)
} 


#' Get feature names of task. 
#' 
#' Target column name is not included.
#'   
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.   
#' @return [\code{character}].
#' @export
getFeatureNames = function(task) {
  #FIXME argument checks currently not done for speed
  return(setdiff(colnames(task$env$data), task$task.desc$target)) 
}

#' Get target column of task. 
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.   
#' @return A \code{factor} for classification or a \code{numeric} for regression.
#' @export
getTargets = function(task) {
  #FIXME argument checks currently not done for speed
  return(task$env$data[, task$task.desc$target])
}

#' Get formula of a task. This is simply \code{target ~ .}. 
#' Note that the environment that always gets attached to a formula is deleted. 
#' @param x [\code{\link{SupervisedTask}} | \code{\link{TaskDesc}}]\cr 
#'   Task or its description object.   
#' @return [\code{formula}]
getFormula = function(x) {
  g = function(target) as.formula(paste(target, "~."))
  if (inherits(x, "TaskDesc"))
    f = g(x$target) 
  else 
    f = g(x$task.desc$target)
  attr(f, ".Environment") = NULL
  return(f)
}

#' Extract data in task.
#'  
#' Useful in \code{\link{trainLearner}} when you add a learning 
#' machine to the package.
#' 
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.   
#' @param subset [\code{integer}]\cr 
#'   Selected cases. 
#'   Default is all cases. 
#' @param features [\code{character}]\cr 
#'   Selected features.  
#'   Default is all features.
#' @param target.extra [\code{logical(1)}]\cr 
#'   Should target vector be returned separately? 
#'   If not, a single data.frame including the target is returned, otherwise a list 
#'   with the feature data.frame and an extra vector for the target column.
#'   Default is FALSE. 
#' @param class.as [\code{character(1)}]\cr
#'   Should target classes be recoded? Useful only for binary classification.
#'   Possible are \dQuote{factor} (do nothing), \dQuote{01}, and \dQuote{-1+1}. 
#'   In the two latter cases the target vector, which is usually a factor, is converted into a numeric vector. 
#'   The positive class is coded as +1 and the negative class either as 0 or -1. 
#'   Default is \dQuote{factor}.
#' @return Either a data.frame or a list with a data.frame \code{data} and a vector \code{target}.
#' @export
getTaskData = function(task, subset, features, target.extra=FALSE, class.as="factor") {
  #FIXME argument checks currently not done for speed
  # maybe recode y
  rec.y = function(y) {
    if (class.as=="01")
      as.numeric(y == task$task.desc$positive)
    else if (class.as=="-1+1")
      2*as.numeric(y == task$task.desc$positive)-1
    else
      y
  }
  
  tn = task$task.desc$target
  ms = missing(subset) || identical(subset, 1:task$task.desc$size)
  mf = missing(features) || identical(features, getFeatureNames(task))
  
  if (target.extra) {
    list(
      data = 
        if (ms && mf) 
        {d=task$env$data;d[,tn]=NULL;d} 
        else if (ms)
          task$env$data[,features,drop=FALSE]
        else if (mf)
        {d=task$env$data[subset,,drop=FALSE];d[,tn]=NULL;d} 
        else
          task$env$data[subset,features,drop=FALSE],
      target = 
        if (ms)
          rec.y(getTargets(task))
        else
          rec.y(getTargets(task)[subset])
    )
  } else {
    d = 
      if (ms && mf) 
        task$env$data 
      else if (ms)
        task$env$data[,c(features, tn),drop=FALSE]
      else if (mf)
        task$env$data[subset,,drop=FALSE]
      else
        task$env$data[subset,features,drop=FALSE]
    if (class.as != "factor")
      d[,tn] = rec.y(d[, tn])
    return(d)
  }
}

#' Subset data in task. 
#' 
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.   
#' @param subset [\code{integer}]\cr 
#'   Selected cases
#'   Default is all cases. 
#' @param features [\code{character}]\cr 
#'   Selected features. Note that the target column is always included in the returned subset. 
#'   Default is all input features. 
#' @return \code{\link{SupervisedTask}} with changed data.
#' @export
subsetTask = function(task, subset, features) {
  #FIXME argument checks currently not done for speed
  if (missing(subset))
    subset = 1:task$task.desc$size
  if (missing(features))
    features = getFeatureNames(task)
  task = changeData(task, getTaskData(task, subset, features))
  if (task$task.desc$has.blocking)
    task$blocking = task$blocking[subset]
  return(task)
}