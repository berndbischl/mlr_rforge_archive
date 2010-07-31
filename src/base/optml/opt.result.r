#' Container for results of hyperparameter tuning or variable selection.
#' Contains the obtained optimal parameter vector, its performance values
#' and the optimization path which lead there. It might also optionally
#' contain a wrapped.model, which was fitted by using the optimal
#' parameters. 
#'
#' Getter.\cr
#' 
#' \describe{
#'   \item{par [list | character]}{Named list of hyperparameter values
#'     or character vector of variables, id#entified as optimal.}
#'   \item{perf [numeric]}{Performance values of 'par'.}
#'   \item{path [list | data.frame]. Optional parameters: as.data.frame}{
#'      Optimization path. Can be converte#d to a data.frame if as.data.frame is TRUE.}
#'   \item{model [wrapped.model]}{Model fitted with settings in 'par'. Will
#'     be NULL, if fitting was not req#uested.}
#' }
#' 
#' @exportClass opt.result
#' @title Optimization result.
#' @seealso \code{\link{tune}}, \code{\link{varsel}} 
setClass(
		"opt.result",
		contains = c("object"),
		representation = representation(
				learner = "learner",
				control = "opt.control",
				opt = "list",
				path = "list",
				model = "wrapped.model"
		)
)

##' Constructor.
setMethod(
		f = "initialize",
		signature = signature("opt.result"),
		def = function(.Object, control, opt, path) {
			if (missing(control))
				return(.Object)
			.Object@control = control 			
			.Object@opt = opt
			if (control["path"])
				.Object@path = path 			
			return(.Object)
		}
)

##' @rdname opt.result-class
setMethod(
		f = "[",
		signature = signature("opt.result"),
		def = function(x,i,j,...,drop) {
			args = list(...)
			if (i == "par") {
				return(x@opt$par)
			}
			if (i == "opt.type"){
				return(x@control["opt.type"])
			}
			if (i == "tuned.par"){
				if (x["opt.type"] != "tune")
					return(NULL)
				return(x["par"])
			}
			if (i == "sel.var"){
				if (x["opt.type"] != "varsel")
					return(NULL)
				return(x["par"])
			}
			if (i == "perf") {
				return(x@opt$perf)
			}
      if (i == "learner") {
        return(make.learner(x@learner, parset=x@opt$par))
      }
			if (i == "path") {
				ys = x@path
				if (!is.null(args$as.data.frame) && args$as.data.frame) {
					ys = path2dataframe(ys)			
				}
				return(ys)
			}
			callNextMethod(x,i,j,...,drop=drop)
		}
)

##' @rdname to.string
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
