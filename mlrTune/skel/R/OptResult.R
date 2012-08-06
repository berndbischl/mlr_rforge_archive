#' Optimization result.
#' 
#' Container for results of hyperparameter tuning and feature selection.
#' Contains the obtained point in search space, its performance values
#' and the optimization path which lead there. 
#'
#' Object members:
#' \describe{
#' \item{learner [\code{\link[mlr]{Learner}}]}{Learner that was optimized.}
#' \item{control [\code{\link{TuneControl}}]}{ Control object from tuning.}
#' \item{x [\code{list} | \code{character}]}{Named list of hyperparameter values or character vector of variables, identified as optimal.}
#' \item{y [\code{numeric}]}{Performance values for optimal \code{x}.}
#' \item{opt.path [\code{\link[ParamHelpers]{OptPath}}]}{Optimization path which lead to \code{x}.}
#' }
#' @name OptResult
#' @rdname OptResult
NULL

makeOptResult = function(learner, control, x, y, opt.path) {
  structure(list(
    learner = learner,       
    control = control,			
		x = x,
    y = y,
    opt.path = opt.path
  ), class=c("TuneResult", "OptResult"))
}

#S3method print OptResult
print.OptResult = function(x, ...) {
  catf("Tune result:")
  catf("Op. pars: %s", paramValueToString(x$opt.path$par.set, x$x))
  catf("%s", mlr:::perfsToString(x$y))
}
