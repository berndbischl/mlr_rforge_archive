#' Container for results of hyperparameter tuning or variable selection.
#' Contains the obtained optimal parameter vector, its performance values
#' and the optimization path which lead there. It might also optionally
#' contain a wrapped.model, which was fitted by using the optimal
#' parameters. 
#'
#' Getter.\cr
#' 
#' \describe{
#'  \item{opt.type [string]}{Currently 'tune' or 'varsel'.}
#'  \item{par [list | character]}{Named list of hyperparameter values or character vector of variables, identified as optimal.}
#'  \item{perf [numeric]}{Performance values of 'par'.}
#'  \item{path [list | data.frame]. Optional parameters: as.data.frame}{Optimization path. Can be converted to a data.frame if as.data.frame is TRUE.}
#'  \item{model [\code{\linkS4class{wrapped.model}}]}{Model fitted with settings in 'par'. Will be NULL, if fitting was not requested.}
#'  \item{learner [\code{\linkS4class{wrapped.model}}]}{Learner with settings in 'par'. Currently only supported for hyperparameter tuning.}
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
				par = "list",
        y = "numeric",
				path = "opt.path",
				model = "wrapped.model"
		)
)

##' Constructor.
setMethod(
		f = "initialize",
		signature = signature("opt.result"),
		def = function(.Object, control, par, y, path) {
			if (missing(control))
				return(.Object)
			.Object@control = control 			
			.Object@par = par
      .Object@y = y
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
				return(x@par)
			}
      if (i == "par.string") {
        if (x["opt.type"] == "tune") {
          return(par.vals.string(x["par"], x@learner))
        } else {
          return(paste(length(x["par"]), "sel. vars"))
        }
      }
			if (i == "opt.type"){
				return(x@control["opt.type"])
			}
			if (i == "perf") {
				return(x@opt$perf)
			}
			if (i == "learner") {
				if (x["opt.type"] != "tune")
					return(NULL)
				wl = set.hyper.pars(x@learner, x["par"])
				return(wl)
			}
			if (i == "path") {
				ys = x@path
				if (!is.null(args$as.data.frame) && args$as.data.frame) {
					ys = path2dataframe(ys)			
				}
				return(ys)
			}
			if (i == "model"){
				if (is.empty(x@model))
					return(NULL)
        else
				  return(x@model)
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
							"Opt. pars: ", x["par.string"], "\n",
							paste(capture.output(x["perf"]), collapse="\n"),"\n",
							sep=""
					)
			)
		}
)
