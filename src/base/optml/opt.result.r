#' Container for results of hyperparameter tuning or variable selection.    
#' Contains the obtained optimal parameter vector, its performance values and the optimization path
#' which lead there.
#' It might also optionally contain a wrapped.model, which was fitted by using the optimal parameters. 
#'
#' #' Getter.\cr
#' 
#' \describe{
#'  \item{par [list | character]}{Named list of hyperparameter values or character vector of variables, identified as optimal.}
#'  \item{perf [numeric]}{Performance values of 'par'.}
#'  \item{path [list | data.frame]. Optional parameters: as.data.frame}{Optimization path. Can be converted to a data.frame if as.data.frame is TRUE.}
#'  \item{model [wrapped.model]}{Model fitted with settings in 'par'. Will be NULL, if fitting was not requested.}
#' }
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

#' @rdname opt.result-class

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
