#' @include object.r
roxygen()
#' @include TaskDesc.R
roxygen()

#' A machine learning task. 
#' It encapsulates the data and specifies - through its subclasses - the type of the task (either classification or regression), 
#' the target variable and other details of the problem. 
#'  
#' @slot dataenv Environment where data for the task are stored. Use \code{\link{getData}} in order to assess the data.
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


setClass(
		"LearnTask",
		contains = c("object"),
		representation = representation(
				dataenv = "environment",
				weights = "numeric",
				blocking = "factor",
				desc = "TaskDesc"
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("LearnTask"),
		def = function(.Object, data, weights, blocking, task.desc) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			if(missing(data))
				return(make.empty(.Object))					
			
			.Object@dataenv = new.env()
      .Object@dataenv$data = data
      .Object@weights = weights
      .Object@blocking = blocking
			.Object@desc = task.desc
			
			return(.Object)
		}
)

#' @rdname LearnTask-class

setMethod(
		f = "[",
		signature = signature("LearnTask"),
		def = function(x,i,j,...,drop) {
			check.getter(x,i,j,...,drop)
			args = list(...)
			argnames = names(args)
			
			td = x@desc
      
			if (i == "blocking") {
				if (!td["has.blocking"])
					return(NULL)
				return(x@blocking)
			}
			y = td[i]
			if (!is.null(y))
				return(y)
			
			callNextMethod()
		}
)


#' Extract data in task. Useful in \code{\link{trainLearner}} when you add a learning 
#' machine to the package.
#' 
#' @param task [\code{\linkS4class{LearnTask}}]\cr 
#'   Learning task.   
#' @param subset [integer] \cr 
#'   Selected cases. Default is all cases. 
#' @param vars [character] \cr 
#'   Selected inputs. 
#' @param target.extra [boolean] \cr 
#'   Should target vector be returned separately? 
#'   If not, a single data.frame including the target is returned, otherwise a list 
#'   with the input data.frame and and an extra vector for the targets.
#'   Default is FALSE. 
#' @param class.as [\code{character(1)}] \cr
#'   Should target classes be recoded? Only for binary classification.
#'   Possible are \dQuote{factor} (do nothing), \dQuote{01}, and \dQuote{-1+1}. 
#'   In the two latter cases the target vector, which is usually a factor, is converted into a numeric vector. 
#'   The positive class is coded as +1 and the negative class either as 0 or -1. 
#'   Default is \dQuote{factor}.
#'    
#' @return Either a data.frame or a list with data.frame \code{data} and vector \code{target}.
#'
#' @export
#' @rdname getData
#' @title Extract data in task. 
# todo: test
getData = function(task, subset, vars, target.extra=FALSE, class.as="factor") {
  
  # maybe recode y
  rec.y = function(y) {
    if (class.as=="01")
      as.numeric(y == task@desc@positive)
    else if (class.as=="-1+1")
      2*as.numeric(y == task@desc@positive)-1
    else
      y
  }
  
  tn = task@desc@target
  ms = missing(subset) || identical(subset, 1:task["size"])
  mv = missing(vars) || identical(vars, getFeatureNames(task))
  
  if (target.extra) {
    list(
      data = 
        if (ms && mv) 
          {d=task@dataenv$data;d[,tn]=NULL;d} 
        else if (ms)
          task@dataenv$data[,vars,drop=FALSE]
        else if (mv)
          {d=task@dataenv$data[subset,,drop=FALSE];d[,tn]=NULL;d} 
        else
          task@dataenv$data[subset,vars,drop=FALSE],
      target = 
        if (ms)
          rec.y(getTargets(task))
        else
          rec.y(getTargets(task)[subset])
    )
  } else {
    d = 
      if (ms && mv) 
        task@dataenv$data 
      else if (ms)
        task@dataenv$data[,c(vars, tn),drop=FALSE]
      else if (mv)
        task@dataenv$data[subset,,drop=FALSE]
      else
        task@dataenv$data[subset,vars,drop=FALSE]
    if (class.as != "factor")
      d[,tn] = rec.y(d[, tn])
    return(d)
  }
}

#' Subset data in task. 
#' 
#' @param x [\code{\linkS4class{LearnTask}}]\cr 
#'   Learning task.   
#' @param subset [integer] \cr 
#'   Selected cases. Default is all cases. 
#' @param vars [character] \cr 
#'   Selected inputs. Note that target feature is always included! Default is all input variables. 
#' @return \code{\linkS4class{LearnTask}} with changed data.
#'
#' @export
#' @rdname subset
#' @seealso \code{\link{getData}} 
#' @title Subset data in task.

setMethod(
  f = "subset",
  signature = signature(x="LearnTask"),
  def = function(x, subset, vars) {
    x = changeData(x, getData(x, subset, vars))
    if (!missing(subset)) {
      x@blocking = x@blocking[subset]
      x@weights = x@weights[subset]
    }  
    return(x)
  }
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
setGeneric(name = "getFeatureNames", def = function(task) standardGeneric("getFeatureNames"))
#' @rdname getFeatureNames
setMethod(
  f = "getFeatureNames",
  signature = signature(task="LearnTask"), 
  def = function(task) {
    setdiff(colnames(task@dataenv$data), task@desc@target)
  } 
)


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

#' Get class levels of task. 
#' @param task [\code{\linkS4class{TaskDesc}} | \code{\linkS4class{ClassifTask}} | \code{\linkS4class{WrappedModel}} ]\cr 
#'   Classification task or its description object.   
#' @return [character]
#' @rdname getClassLevels
#' @exportMethod getClassLevels
setGeneric(name = "getClassLevels", def = function(x) standardGeneric("getClassLevels"))
#' @rdname getClassLevels
setMethod(
  f = "getClassLevels",
  signature = signature(x="ClassifTask"), 
  def = function(x) {
    if (x@desc@type != "classif")
      stop("Description is not for a classification task!")
    names(x@desc@class.dist)
  } 
)
#' @rdname getClassLevels
setMethod(
  f = "getClassLevels",
  signature = signature(x="TaskDesc"), 
  def = function(x) {
    if (x@type != "classif")
      stop("Description is not for a classification task!")
    names(x@class.dist)
  } 
)
#' @rdname getClassLevels
setMethod(
  f = "getClassLevels",
  signature = signature(x="WrappedModel"), 
  def = function(x) {
    if (x@desc@type != "classif")
      stop("Description is not for a classification task!")
    names(x@desc@class.dist)
  } 
)

#' @rdname getClassLevels
setMethod(
  f = "getClassLevels",
  signature = signature(x="Prediction"), 
  def = function(x) {
    if (x@desc@type != "classif")
      stop("Description is not for a classification task!")
    names(x@desc@class.dist)
  } 
)


#' Get formula of a task. This is simply \code{target ~ .}. 
#' Note that the environment that always gets attached to a formula is deleted. 
#' @param task [\code{\linkS4class{LearnTask}} | \code{\linkS4class{TaskDesc}}]\cr 
#'   Task or its description object.   
#' @return [\code{formula}]
#' @rdname getFormula
#' @exportMethod getFormula
setGeneric(name = "getFormula", def = function(x) standardGeneric("getFormula"))
#' @rdname getFormula
setMethod(
  f = "getFormula",
  signature = signature(x="LearnTask"), 
  def = function(x) {
    getFormula(x@desc) 
  } 
)
#' @rdname getFormula
setMethod(
  f = "getFormula",
  signature = signature(x="TaskDesc"), 
  def = function(x) {
    f = paste(x@target, "~.")
    attr(f, ".Environment") = NULL
    return(f)
  } 
)
