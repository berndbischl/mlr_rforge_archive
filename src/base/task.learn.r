#' @include object.r
roxygen()
#' @include task.desc.r
roxygen()
#' @include prepare.df.r
roxygen()

#' General description object for a machine learning task. 
#' It encapsulates the data and specifies - through its subclasses - the type of the task (either classification or regression), 
#' the target variable and other details of the problem. As this is just an abstract base class, 
#' you should not instantiate it directly but use \code{\link{make.task}}.
#'  
#' Getter.\cr
#' Note that all getters of \code{\linkS4class{task.desc}} can also be used, as they internally encapsulate some information of the task. 
#' 
#' \describe{
#' 	\item{data [data.frame]}{Encapsulated data.}
#'  \item{input.names [character]}{The names of the input variables.}
#'  \item{targets [character]}{Target column of data.}
#'  \item{weights [numeric]}{Case weights are returned. NULL if no weights were set.}
#'  \item{blocking [factor]}{Observations with the same blocking level "belong together". Specifically, they are either put all in the training or the test set during a resampling iteration. NULL if no blocking was set.}
#'	\item{prepare.control [\code{\linkS4class{prepare.control}}]}{Control object used for preparing the data.frame.}
#' }
#' 
#' Subclasses: \code{\linkS4class{classif.task}}, \code{\linkS4class{regr.task}}
#' 
#' @exportClass learn.task
#' @seealso \code{\link{make.task}} 
#' @title Base class for learning tasks.


setClass(
		"learn.task",
		contains = c("object"),
		representation = representation(
				data = "data.frame",
				weights = "numeric",
				blocking = "factor",
        control = "prepare.control",
				task.desc = "task.desc"
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("learn.task"),
		def = function(.Object, data, weights, blocking, control, task.desc) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			if(missing(data))
				return(make.empty(.Object))					
			
			.Object@data = data
			.Object@weights = weights
      .Object@blocking = blocking
      .Object@control = control
			.Object@task.desc = task.desc
			
			return(.Object)
		}
)

#' @rdname learn.task-class

setMethod(
		f = "[",
		signature = signature("learn.task"),
		def = function(x,i,j,...,drop) {
			check.getter(x,i,j,...,drop)
			args = list(...)
			argnames = names(args)
			
			td = x@task.desc
      
			if (i == "input.names"){
				return(setdiff(colnames(x@data), x["target"]))
			}
			if (i == "targets") {
				return(x@data[, x["target"]])
			}
      if (i == "weights") {
				if (!td["has.weights"])
					return(NULL)
				return(x@weights)
			}
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

# todo: maybe dont do this check for speed reasons?
## reduce data to subset and selected vars
#x = !vars %in% task["input.names"]
#if (sum(x) > 0)
#  stop("Trying to subset with vars which are not inputs: ", paste(vars[x], collapse=","))

#' Subset data in task. 
#' 
#' @param x [\code{\linkS4class{learn.task}}]\cr 
#'   Specifies learning task. If this is passed, data from this task is predicted.   
#' @param subset [integer] \cr 
#'   Index vector to subset the data in the task to use for prediction. 
#' @param vars [character] \cr 
#'   New observations which should be predicted. Alternatively pass this instead of task. 
#' @return \code{\linkS4class{learn.task}} with changed data.
#'
#' @export
#' @rdname subset
#' @importFrom base subset
#' @seealso \code{\link{get.data}} 
#' @title Subset data in task.

setMethod(
  f = "subset",
  signature = signature(x="learn.task"),
  def = function(x, subset=1:x["size"], vars) {
    x = change.data(x, get.data(x, subset, vars))
    x@blocking = x@blocking[subset]
    x@weights = x@weights[subset]
    return(x)
  }
)

change.data = function(task, data) {
  task@data = data
  task@task.desc = new("task.desc", task@data, task["target"], class(task), task["id"], 
    task["has.weights"], task["has.blocking"], task["costs"], task["positive"])
  return(task)
} 

