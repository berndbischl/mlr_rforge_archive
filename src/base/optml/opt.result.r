
# A list. Might contain some additional information from the optimizer and at least:
#'   \item{par}{Named list of best found hyperparamters.}
#'   \item{perf}{Best found performance value.}
#'   \item{model}{Fitted model on complete data set - if requested.}

setClass(
		"opt.result",
		contains = c("object"),
		representation = representation(
				opt = "list",
				path = "list",
				model = "wrapped.model"
		)
)

#' Getter.
#' 
#' @param x object
#' @param i [character]
#' \describe{
#'   \item{class.levels}{All possible class values.}
#'   \item{class.nr}{Number of different classes.}
#' }
#' @rdname opr.result-class

setMethod(
		f = "[",
		signature = signature("opt.result"),
		def = function(x,i,j,...,drop) {
			args = list(...)
			if (i == "par") {
				return(x@opt$par)
			}
			if (i == "perf") {
				return(x@opt$perf)
			}
			if (i == "path") {
				ys = x@path
				if (!is.null(args$as.data.frame) && args$as.data.frame) {
					ys = lapply(ys, function(y) cbind(
										as.data.frame(as.list(y$par), stringsAsFactors = FALSE), 
										as.data.frame(as.list(y$perf)),
										evals=y$evals, op=y$event, accept=y$accept,
										stringsAsFactors = FALSE
					))
					ys = Reduce(rbind, ys)
					rownames(ys) = NULL
				}
				return(ys)
			}
			callNextMethod(x,i,j,...,drop=drop)
		}
)



#' @rdname to.string
setMethod(
		f = "to.string",
		signature = signature("opt.result"),
		def = function(x) {
			return(
					paste(
							"Optimization result: \n",
							paste(capture.output(x["par"]), collapse="\n"),
							"\n",
							paste(capture.output(x["perf"]), collapse="\n"),
							"\n",
							sep=""
					)
			)
		}
)
