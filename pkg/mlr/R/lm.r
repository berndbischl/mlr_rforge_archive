#' @include wrapped.learner.regr.r 
roxygen()

#' @export
setClass(
		"stats.lm", 
		contains = c("wrapped.learner.regr")
)



setMethod(
		f = "initialize",
		signature = signature("stats.lm"),
		def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "lm"
			predict.fct <- predict
			
			desc = new("regr.props",
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			.Object <- callNextMethod(.Object, learner.name="Linear Regression", learner.pack="stats",
					learner.model.class="lm", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=list(),
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					learner.props=desc)
			return(.Object)
		}
)






