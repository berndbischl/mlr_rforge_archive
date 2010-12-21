#' @include object.r
roxygen()
#' @include data.desc.r
roxygen()
#' @include task.desc.r
roxygen()

#' General description object for a machine learning task. 
#' It encapsulates the data and specifies - through its subclasses - the type of the task (either classification or regression), 
#' the target variable and other details of the problem. As this is just an abstract base class, 
#' you should not instantiate it directly but use \code{\link{make.task}.
#'  
#' Getter.\cr
#' Note that all getters of \code{\linkS4class{task.desc}} and \code{\linkS4class{data.desc}} can also be used, as they internally encapsulate some information of the task. 
#' 
#' \describe{
#' 	\item{data [data.frame] Optional parameters: exclude=TRUE}{Encapsulated data. Excluded variables are excluded iff \code{exclude} == TRUE.}
#'  \item{input.names [character]}{The names of the input variables (without excluded variables).}
#'  \item{targets [character]}{Target column of data.}
#'  \item{weights [numeric]}{Case weights are returned. NULL if no weights were set.}
#'  \item{blocking [factor]}{Observations with the same blocking level "belong together". Specifically, they are either put all in the training or the test set during a resampling iteration. NULL if no blocking was set.}
#'	\item{prepare.control [\code{\linkS4class{prepare.control}}]}{Control object used for preparing the data.frame.}
#' }
#' 
#' Subclasses: \code{\linkS4class{classif.task}}, \code{\linkS4class{regr.task}}
#' 
#' @exportClass learn.task
#' @seealso \code{\link{make.task}}, 
#' @title Base class for learning tasks.


setClass(
		"learn.task",
		contains = c("object"),
		representation = representation(
				data = "data.frame",
				weights = "numeric",
				blocking = "factor",
        control = "prepare.control",
        data.desc = "data.desc",
				task.desc = "task.desc"
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("learn.task"),
		def = function(.Object, data, weights, blocking, control, data.desc, task.desc) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			if(missing(data))
				return(make.empty(.Object))					
			
			.Object@data = data
			.Object@weights = weights
      .Object@blocking = blocking
      .Object@control = control
			.Object@data.desc = data.desc
			.Object@task.desc = task.desc
			
#			.Object@data.desc <- make.data.desc(.Object["data"], target)
			
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
			
			dd = x@data.desc
			td = x@task.desc
      exc = args$exclude
      exc = if (is.null(exc)) TRUE else exc
      
      
      
			if (i == "input.names"){
				return(setdiff(colnames(x@data), c(x["exclude"], x["target"])))
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
			if (i == "data"){
        if (exc) {
          return(x@data[, setdiff(colnames(x@data), x["exclude"])])
        } else
          return(x@data)				
			}
			y = td[i]
			if (!is.null(y))
				return(y)
			y = dd[i]
			if (!is.null(y))
				return(y)
			
			callNextMethod()
		}
)
