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
				dataenv = "environment",
				weights = "numeric",
				blocking = "factor",
        control = "prepare.control",
				desc = "task.desc"
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
			
			.Object@dataenv = new.env()
      .Object@dataenv$data = data
      .Object@weights = weights
      .Object@blocking = blocking
      .Object@control = control
			.Object@desc = task.desc
			
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
			
			td = x@desc
      
      if (i == "data"){
        return(x@dataenv$data)
      }
      if (i == "input.names"){
				return(setdiff(colnames(x["data"]), x["target"]))
			}
			if (i == "targets") {
				return(x["data"][, x["target"]])
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


#' Extract data in task. Useful in \code{link{train.learner}} when you add a learning 
#' machine to the package.
#' 
#' @param task [\code{\linkS4class{learn.task}}]\cr 
#'   Learning task.   
#' @param subset [integer] \cr 
#'   Selected cases. Default is all cases. 
#' @param vars [character] \cr 
#'   Selected inputs. 
#' @param target.extra [boolean] \cr 
#'   Should target vector be returned separately?. 
#'   If not, a single data.frame including the target is returned, otherwise a list 
#'   with the input data.frame and and an extra vector for he targets.
#'   Default is FALSE. 
#' @param class.as [string] \cr
#'   Should target classes be recoded? Only for binary classification.
#'   Possible are "factor" (do nothing), "01", and "-1+1". 
#'   Default is "factor".
#'    
#' @return Either a data.frame or a list with data.frame \code{data} and vector \code{target}.
#'
#' @export
#' @rdname subset
#' @seealso \code{\link{get.data}} 
#' @title Extract data in task. 
# todo: test
get.data = function(task, subset, vars, target.extra=FALSE, class.as="factor") {
  
  # maybe recode y
  rec.y = function(y) {
    if (class.as=="01")
      as.numeric(y == task["positive"])
    else if (class.as=="-1+1")
      2*as.numeric(y == task["positive"])-1
    else
      y
  }
  
  tn = task["target"]
  ms = missing(subset) || identical(subset, 1:task["size"])
  mv = missing(vars) || identical(vars, task["input.names"])
  
  if (target.extra) {
    list(
      data = 
        if (ms && mv) 
          {d=task["data"];d[,tn]=NULL;d} 
        else if (ms)
          task["data"][,vars,drop=FALSE]
        else if (mv)
          {d=task["data"][subset,,drop=FALSE];d[,tn]=NULL;d} 
        else
          task["data"][subset,vars,drop=FALSE],
      target = 
        if (ms)
          rec.y(task["targets"])
        else
          rec.y(task["targets"][subset])
    )
  } else {
    d = 
      if (ms && mv) 
        task["data"] 
      else if (ms)
        task["data"][,c(vars, tn),drop=FALSE]
      else if (mv)
        task["data"][subset,,drop=FALSE]
      else
        task["data"][subset,vars,drop=FALSE]
    if (class.as != "factor")
      d[,tn] = rec.y(d[, tn])
    return(d)
  }
}

#' Subset data in task. 
#' 
#' @param x [\code{\linkS4class{learn.task}}]\cr 
#'   Learning task.   
#' @param subset [integer] \cr 
#'   Selected cases. Default is all cases. 
#' @param vars [character] \cr 
#'   Selected inputs. Note that target feature is always included! Default is all vars. 
#' @return \code{\linkS4class{learn.task}} with changed data.
#'
#' @export
#' @rdname subset
#' @seealso \code{\link{get.data}} 
#' @title Subset data in task.

setMethod(
  f = "subset",
  signature = signature(x="learn.task"),
  def = function(x, subset, vars) {
    x = change.data(x, get.data(x, subset, vars))
    if (!missing(subset)) {
      x@blocking = x@blocking[subset]
      x@weights = x@weights[subset]
    }  
    return(x)
  }
)

# we create a new env, so the reference is not changed
change.data = function(task, data) {
  task@dataenv = new.env()
  task@dataenv$data = data
  task@desc = new("task.desc", task["data"], task["target"], class(task), task["id"], 
    task["has.weights"], task["has.blocking"], task["costs"], task["positive"])
  return(task)
} 

