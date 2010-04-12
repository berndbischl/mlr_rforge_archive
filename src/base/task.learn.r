#' @include object.r
roxygen()
#' @include data.desc.r
roxygen()
#' @include task.desc.r
roxygen()

#' A learning task is a general description object for a machine learning experiment. 
#' It wraps the data source and specifies - through its subclasses - the type of the task (e.g. classification or regression), 
#' the target variable and other details of the problem. As this is just an abstract base class, 
#' you should not instantiate it directly but use the inheriting classes and their factory methods.
#' 
#' @exportClass learn.task
#' @seealso \code{\link{make.task}} \code{\link{make.task}}
#' @title learn.task


setClass(
		"learn.task",
		contains = c("object"),
		representation = representation(
				data = "data.frame",
				weights = "numeric",
				data.desc = "data.desc",
				task.desc = "task.desc"
		)
)


#---------------- constructor---- -----------------------------------------------------

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("learn.task"),
		def = function(.Object, data, weights, data.desc, task.desc) {
			
			# constructor is called in setClass of inheriting classes 
			# wtf chambers, wtf!
			if(missing(data))
				return(.Object)					
			


			.Object@data = data
			.Object@weights = weights
			.Object@data.desc = data.desc
			.Object@task.desc = task.desc
			
#			.Object@data.desc <- make.data.desc(.Object["data"], target)
			
			return(.Object)
		}
)

#' Getter.
#' @param x learn.task object
#' @param i [character]
#' \describe{
#'   \item{target.name}{The name of the target variable.}
#'   \item{target.col}{The column number of the target variable.}
#'   \item{targets}{If j is missing all target values are returned. Otherwise they are indexed by j.}
#'   \item{input.names}{The names of the input variables.}
#' }
#' @param j [integer] \cr See above, i == "targets".
#' 
#' @rdname learn.task-class

setMethod(
		f = "[",
		signature = signature("learn.task"),
		def = function(x,i,j,...,drop) {
			args = list(...)
			argnames = names(args)
			
			dd = x@data.desc
			td = x@task.desc
			
			if (i == "target.name") {
				return(td["target"])
			}
			if (i == "targets") {
				if (missing(j))
					j = 1:nrow(x@data)
				return(x@data[j, x["target.name"]])
			}
			if (i == "input.names"){
				return(setdiff(colnames(x@data), c(x["excluded"], x["target.name"])))
			}
			
			if (i == "has.weights"){
				return(length(x["weights"]) > 0)
			}
			
			if (i == "data"){
				if (missing(j))
					j = 1:nrow(x@data)
				if ("excluded" %in% argnames)
					v = colnames(x@data)
				else 
					v = setdiff(colnames(x@data), x["excluded"])
				if ("select" %in% argnames)
					v = args$select
				if (missing(drop))
					drop = (length(v) == 1)
				return(x@data[j, v, drop=drop])				
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



#---------------- restrict.learn.task -----------------------------------------------------

restrict.learn.task <- function(learn.task, subset) {
	learn.task@data <- learn.task@data[subset,]
	return(learn.task)
}


