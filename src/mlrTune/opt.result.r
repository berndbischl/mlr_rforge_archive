#' @include OptPath.R
roxygen()
#' @include OptControl.R
roxygen()

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
				learner = "Learner",
				control = "OptControl",
				x = "ANY",
        y = "numeric",
				path = "OptPath"
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


##' @rdname to.string
setMethod(
		f = "to.string",
		signature = signature("opt.result"),
		def = function(x) {
      s = if (is(x@control, "TuneControl")) 
        valToString(getParameterSet(x@learner), x@x)
      else 
        paste(length(x@x), "sel. vars")
      return(
					paste(
							"Opt. pars: ", s, "\n",
							paste(paste(names(x@y), formatC(x@y, digits=3), sep="="), collapse=" "), 
							sep=""
					)
			)
		}
)
