#' @include wrapped.learner.regr.r 
roxygen()

#' Wrapped learner for Boosting with Regression Trees from package \code{mboost} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{mstop}}{Integer, giving the number of initial boosting iterations.}			
#' 		\item{\code{nu}}{Double (between 0 and 1), defining the step size or shrinkage parameter.}
#' 		\item{\code{constraint}}{Logical, indicating whether the working responses should be restricted to (-1, +1).}
#' 		\item{\code{risk}}{Character, indicating how the empirical risk should be computed for each boosting iteration.}			
#' 		\item{\code{center}}{Logical, indicating if the numerical covariates should be mean centered before fitting.}
#' }
#' @title blackboost.regr
#' @seealso \code{\link[mboost]{blackboost}}, \code{\link[mboost]{boost_control}}
#' @export
setClass(
		"blackboost.regr", 
		contains = c("wrapped.learner.regr")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Boosting Constructor
setMethod(
		f = "initialize",
		signature = signature("blackboost.regr"),
		def = function(.Object, data, formula) {
			
			.Object <- callNextMethod(.Object, learner.name="blackboost", learner.pack="mboost",
					train.fct="blackboost")
			return(.Object)
		}
)





