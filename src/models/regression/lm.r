#' @include wrapped.learner.regr.r
roxygen()


#' Wrapped learner for Linear Models from package \code{stats} for regression problems.
#' @title stats.lm
#' @seealso \code{\link[stats]{lm}}
#' @export
setClass(
		# name lm is sealed
		"stats.lm", 
		contains = c("wrapped.learner.regr")
)


#' Constructor.
#' @title LM Constructor
setMethod(
		f = "initialize",
		signature = signature("stats.lm"),
		def = function(.Object, ...) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			callNextMethod(.Object, learner.name="Linear Regression", learner.pack="stats", learner.props=desc, ...)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="stats.lm", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, ...) {
			f = as.formula(paste(.targetvar, "~."))
			lm(f, data=.data, weights=.weights, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "stats.lm", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, ...) {
			predict(.wrapped.model["learner.model"], newdata=.newdata, ...)
		}
)	





