#' @include task.learn.r
roxygen()

#' General description object for a classification experiment.   
#' Instantiate it by using its factory method.
#' 
#' @exportClass classif.task
#' @title Classification task.
#' @seealso \code{\link{make.task}}


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
		def = function(.Object, id, label, target, data, excluded, weights, costs, positive) {
			if (missing(data))
				return(.Object)
			
			data = prep.classif.data(data, target, excluded)			
			dd = new("data.desc", data=data, target=target, excluded=excluded)
			
			n = dd["class.nr"]
			levs = dd["class.levels"]
					
			# init positive
			pos = positive 
			neg = as.character(NA)
			if (n == 2) {
				if (is.na(pos)) {
					pos = levs[1] 					
				}
				else {
					if (!(pos %in% levs))
						stop(paste("Trying to set a positive class", pos, "which is not a value of the target variable:", paste(levs, collapse=",")))
				}
				neg = setdiff(levs, pos)
			} else {
				if (!is.na(pos))
					stop("Cannot set a positive class for a multiclass problem!")
			}
			
			# check costs if passed
			if (!all(dim(costs) == 0)) {
				if (!is.matrix(costs))
					stop("costs has to be a matrix!")
				if (any(dim(costs) != n))
					stop("Dimensions of costs has to be the same as number of classes!")
				rns = rownames(costs)
				cns = colnames(costs)
				if (!setequal(rns, levs) || !setequal(cns, levs))
					stop("Row and column names of cost matrix have to equal class levels!")
			}			

			td = new("task.desc", task.class="classif.task", id=id, label=label, target=target, positive=pos, negative=neg, excluded=excluded, costs=costs)			

			callNextMethod(.Object, data=data, weights=weights, data.desc=dd, task.desc=td)
		}
)


#' @rdname to.string
setMethod(
		f = "to.string",
		signature = signature("classif.task"),
		def = function(x) {
			return(
					paste(
							"Classification problem ", x["id"], "\n",
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


