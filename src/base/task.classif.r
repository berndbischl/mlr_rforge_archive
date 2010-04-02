#' @include task.learn.r
roxygen()

#' General description object for a classification experiment.   
#' Instantiate it by using its factory method.
#' 
#' @slot costs Matrix of misclassification costs. Default is zero-one loss. 
#' 
#' @exportClass classif.task
#' @title classif.task
#' @seealso make.regr.task 


setClass(
		"classif.task",
		contains = c("learn.task")
)



#---------------- constructor---- -----------------------------------------------------

#' Constructor.
#' @title classif.task constructor

setMethod(
		f = "initialize",
		signature = signature("classif.task"),
		def = function(.Object, name, target, data, excluded, weights, costs, positive) {
			
			
			if (missing(data))
				return(.Object)
			
			check.task(data, target)
			data = prep.classif.data(data, target, excluded)			
			dd = new("data.desc", data=data, target=target, excluded=excluded)
			
			n = dd["class.nr"]
			levs = dd["class.levels"]
			
			# init costs
			if (dim(costs)==c(1,1) && is.na(costs)) {
				costs = matrix(1,n,n) - diag(1,n)
			}
			
			# init positive
			pos = positive 
			neg = as.character(NA)
			if (n == 2) {
				if (is.na(pos)) {
					pos = levs[1] 					
				}
				else {
					if (!(pos %in% levs))
						stop(paste("Trying to set a positive class", .Object@positive, "which is not a value of the target variable:", paste(levs, collapse=",")))
				}
				neg = setdiff(levs, pos)
			} else {
				if (!is.na(pos))
					stop("Cannot set a positive class for a multiclass problem!")
			}
			td = new("task.desc", task.class="classif.task", name=name, target=target, positive=pos, negative=neg, excluded=excluded, weights=weights, costs=costs)			

			callNextMethod(.Object, data=data, data.desc=dd, task.desc=td)
		}
)

#' Getter.
#' @param x classif.task object
#' @param i [character]
#' \describe{
#'   \item{class.levels}{All possible class values.}
#'   \item{class.nr}{Number of different classes.}
#' }
#' @rdname getter,classif.task-method
#' @aliases classif.task.getter getter,classif.task-method
#' @seealso \code{\link{getter,learn.task-method}}
#' @title Getter for classif.task

setMethod(
		f = "[",
		signature = signature("classif.task"),
		def = function(x,i,j,...,drop) {

#			if (i == "class.levels") {
#				return(levels(x["targets"]))
#			}
#			if (i == "class.nr") {
#				return(length(levels(x["targets"])))
#			}
#			if (i == "is.binary") {
#				return(x["class.nr"] == 2)
#			}
#			if (i == "negative") {
#				if (x["is.binary"])
#					return(setdiff(x["class.levels"], x["positive"]))
#				return(NA)
#			}
			# otherwise drop gets lost. bug in S4
			callNextMethod(x,i,j,...,drop=drop)
		}
)


#' Conversion to string.
setMethod(
		f = "to.string",
		signature = signature("classif.task"),
		def = function(x) {
			return(
					paste(
							"Classification problem ", x["name"], "\n",
							to.string(x@data.desc),
							"Classes:", x["class.nr"],
							paste(capture.output(table(x["targets"])), collapse="\n"),
							"\n",
							ifelse(x["is.binary"], paste("Positive class:", x["positive"], "\n"), ""),
							sep=""
					)
			)
		}
)


