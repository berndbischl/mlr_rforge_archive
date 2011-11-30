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
#' @exportClass OptResult
#' @title Optimization result.
#' @seealso \code{\link{tune}}, \code{\link[mlrVarsel]{varsel}} 
setClass(
		"OptResult",
		representation = representation(
				learner = "Learner",
				control = "OptControl",
				x = "ANY",
        y = "numeric",
				path = "ANY"
		)
)

##' Constructor.
setMethod(
		f = "initialize",
		signature = signature("OptResult"),
		def = function(.Object, learner, control, x, y, path) {
			if (missing(control))
				return(.Object)
      .Object@learner = learner       
      .Object@control = control 			
			.Object@x = x
      .Object@y = y
      if (control@path)
				.Object@path = path 			
			return(.Object)
		}
)

#' @rdname undocumented
setMethod(f = "show", signature = signature("OptResult"), def = function(object) {
  s = if (is(object@control, "TuneControl")) 
    paramValueToString(object@path$par.set, object@x)
  else 
    paste(length(object@x), "sel. vars")
  cat("Opt. pars:", s, "\n",
    paste(paste(names(object@y), formatC(object@y, digits=3), sep="="), collapse=" "),
    "\n")
})
