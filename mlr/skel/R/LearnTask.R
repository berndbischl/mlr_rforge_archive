#' A machine learning task. 
#' It encapsulates the data and specifies - through its subclasses - the type of the task (either classification or regression), 
#' the target variable and other details of the problem. 
#'  
#' @slot dataenv Environment where data for the task are stored. Use \code{\link{getData}} in order to access the data.
#' @slot weights Case weights. \code{numeric(0)} if no weights were set.
#' @slot blocking Observations with the same blocking level "belong together". Specifically, they are either put all in the training 
#'   or the test set during a resampling iteration. \code{factor(0)} if no blocking was set.
#' @slot desc An object of class \code{\linkS4class{TaskDesc}} which encapsulates the main information about the task.
#'
#' Subclasses: \code{\linkS4class{ClassifTask}}, \code{\linkS4class{RegrTask}}
#' 
#' @exportClass LearnTask
#' @seealso \code{\link{makeClassifTask}}, \code{\link{makeRegrTask}} 
#' @title Base class for learning tasks.

makeSupervisedTask = function(.Object, data, weights, blocking, task.desc) {
  .Object@dataenv = new.env()
  .Object@dataenv$data = data
  .Object@weights = weights
  .Object@blocking = blocking
  .Object@desc = task.desc
)




# we create a new env, so the reference is not changed
changeData = function(task, data) {
  task@dataenv = new.env()
  task@dataenv$data = data
  d = task@desc
  task@desc = new("TaskDesc", data, d@target, d@type, d@id, 
    d@has.weights, d@has.blocking, d@positive)
  return(task)
} 


#' Get feature names of task. 
#' @param task [\code{\linkS4class{LearnTask}}]\cr 
#'   Learning task.   
#' @return [character].
#' @rdname getFeatureNames
#' @title Get feature names of task.
#' @exportMethod getFeatureNames
getFeatureNames = function(task) {
  setdiff(colnames(task@dataenv$data), task@desc@target) 
}


#' Get target column of task. 
#' @param task [\code{\linkS4class{LearnTask}}]\cr 
#'   Learning task.   
#' @return A factor for classification or a numeric for regression.
#' @rdname getTargets
#' @exportMethod getTargets
setGeneric(name = "getTargets", def = function(task) standardGeneric("getTargets"))
#' @rdname getTargets
setMethod(
  f = "getTargets",
  signature = signature(task="LearnTask"), 
  def = function(task) {
    return(task@dataenv$data[, task@desc@target])
  } 
)

#' Get formula of a task. This is simply \code{target ~ .}. 
#' Note that the environment that always gets attached to a formula is deleted. 
#' @param task [\code{\linkS4class{LearnTask}} | \code{\linkS4class{TaskDesc}}]\cr 
#'   Task or its description object.   
#' @return [\code{formula}]
#' @rdname getFormula
#' @exportMethod getFormula
    getFormula(x@desc) 
  } 
)
getFormula = function(x) {
  if (inherits())
    f = as.formula(paste(x@target, "~."))
    attr(f, ".Environment") = NULL
    return(f)
  } 
)
