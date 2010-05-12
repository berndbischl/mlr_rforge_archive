#' Container for results of hyperparameter tuning or variable selection.    
#' Contains the obtained optimal parameter vector, its performance values and the optimization path
#' which lead there.
#' It might also optionally contain a wrapped.model, which was fitted by using the optimal parameters. 
#' 
#' @exportClass opt.result
#' @title Optimization result.
#' @seealso \code{\link{tune}}, \code{\link{varsel}} 


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
#'   \item{opt}{ List with following elements: 'par' is named list of hyperparamter values or character vector of variables, 'perf' are the performance values of the optimum, 'evals' is the number of function nevaluation needed to find it.}
#'   \item{path}{ Optimization path. List of elements which have the same structure as 'opt'.}
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
										threshold=y$threshold,
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
