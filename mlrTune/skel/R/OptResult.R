#' Tuning result.
#' 
#' Container for results of hyperparameter tuning.
#' Contains the obtained optimal parameter vector, its performance values
#' and the optimization path which lead there. 
#'
#'  learner Learner that was optimized.
#'  control Control object for opimization.
#'  x Named list of hyperparameter values or character vector of variables, identified as optimal.
#'  y Performance values for optimal 'x'.
#'  path Optimization path which lead to 'x'.
#' 
#' @seealso \code{\link{tune}}, \code{\link[mlrVarsel]{varsel}} 

makeOptResult = function(learner, control, x, y, path) {
  structure(list(
    learner = learner,       
    control = control,			
		x = x,
    y = y,
    path = path
  ), class=c("TuneResult, OptResult"))
}

print.OptResult = function(x, ...) {
  catf("Tune result:")
  catf("  Op. pars: %s", paramValueToString(x$path$par.set, x$x))
  catf("  %s", mlr:::perfsToString(x$y))
}
