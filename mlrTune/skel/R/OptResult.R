
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

makeOptResult = function(learner, control, x, y, path) {
  #FIXME do path
  if (control@path)
    path = path
  structure(list(
    learner = learner,       
    control = control,			
		x = x,
    y = y
  ), class="OptResult")
}

print.OptResult = function(x, ...) {
  s = if (inherits(x$control, "TuneControl")) 
    paramValueToString(x$path$par.set, object@x)
  else 
    paste(length(x$x), "sel. vars")
  cat("Opt. pars:", s, "\n",
    paste(paste(names(x$y), formatC(x$y, digits=3), sep="="), collapse=" "),
    "\n")
}
