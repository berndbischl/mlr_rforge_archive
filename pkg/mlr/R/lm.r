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
		def = function(.Object) {
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Linear Regression", learner.pack="stats",
					train.fct=lm, 
					learner.props=desc)
			return(.Object)
		}
)






