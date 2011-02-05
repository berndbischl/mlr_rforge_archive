#' Container for results of hyperparameter tuning or variable selection.
#' Contains the obtained optimal parameter vector, its performance values
#' and the optimization path which lead there. 
#'
#' @slot learner Learner that was optimized.
#' @slot control Control object for opimization.
#' @slot x Named list of hyperparameter values or character vector of variables, identified as optimal.
#' @slot y Performance values for optimal 'x'.
#' @slot path Optimization path which lead to 'x'.
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
				x = "ANY",
        y = "numeric",
				path = "opt.path"
		)
)

##' Constructor.
setMethod(
		f = "initialize",
		signature = signature("opt.result"),
		def = function(.Object, learner, control, x, y, path) {
			if (missing(control))
				return(.Object)
      .Object@learner = learner       
      .Object@control = control 			
			.Object@x = x
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
			if (i == "x") {
				return(x@x)
			}
      if (i == "par.string") {
        if (x["opt.type"] == "tune") {
          return(valToString(x@learner["par.set"], x@x))
        } else {
          return(paste(length(x@x), "sel. vars"))
        }
      }
			if (i == "opt.type"){
				return(x@control["opt.type"])
			}
			if (i == "learner") {
				if (x["opt.type"] != "tune")
					return(NULL)
				wl = set.hyper.pars(x@learner, x@x)
				return(wl)
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
							"Opt. pars: ", x["par.string"], "\n",
							paste(paste(names(x@y), formatC(x@y, digits=3), sep="="), collapse=" "), 
							sep=""
					)
			)
		}
)
