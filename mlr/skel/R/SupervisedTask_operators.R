
#' Get feature names of task. 
#' 
#' Target column name is not included.
#'   
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.   
#' @return [\code{character}].
#' @export
getTaskFeatureNames = function(task) {
  #FIXME argument checks currently not done for speed
  return(setdiff(colnames(task$data), task$task.desc$target)) 
}

#' Get formula of a task. This is simply \code{target ~ .}. 
#' Note that the environment that always gets attached to a formula is deleted. 
#' @param x [\code{\link{SupervisedTask}} | \code{\link{TaskDesc}}]\cr 
#'   Task or its description object.   
#' @return [\code{formula}]
getTaskFormula = function(x) {
  g = function(target) as.formula(paste(target, "~."))
  if (inherits(x, "TaskDesc"))
    f = g(x$target) 
  else 
    f = g(x$task.desc$target)
  attr(f, ".Environment") = NULL
  return(f)
}


#' Get target column of task. 
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   The task.   
#' @return A \code{factor} for classification or a \code{numeric} for regression.
#' @export
getTaskTargets = function(task, subset, recode.y="no") {
  #FIXME argument checks currently not done for speed
  y = task$data[subset, task$task.desc$target]
  recodeY(y, recode.y, task$task.desc$positive)
}

getTaskModelMatrix = function(task, subset) {
  data = task$model.matrix[subset,,drop=FALSE]
  return(data)
}
