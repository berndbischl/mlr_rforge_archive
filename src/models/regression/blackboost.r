#' @include wrapped.learner.regr.r
roxygen()

#' Wrapped learner for Boosting with Regression Trees from package \code{mboost} for regression problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{mstop}}{Integer, giving the number of initial boosting iterations.}			
#' 		\item{\code{nu}}{Double (between 0 and 1), defining the step size or shrinkage parameter.}
#' 		\item{\code{constraint}}{Logical, indicating whether the working response should be restricted to (-1, +1).}
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
		def = function(.Object, data, target) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			callNextMethod(.Object, learner.name="blackboost", learner.pack="mboost", learner.props=desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="blackboost.regr", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, ...) {
			f = as.formula(paste(.targetvar, "~."))
			blackboost(f, data=.data, weights=.weights, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "blackboost.regr", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			predict(.wrapped.model["learner.model"], newdata=.newdata, ...)
		}
)	





