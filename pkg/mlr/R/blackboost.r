#' @include wrapped.learner.regr.r 
roxygen()

#' @export
setClass(
		"blackboost.regr", 
		contains = c("wrapped.learner.regr")
)


#----------------- constructor ---------------------------------------------------------

setMethod(
		f = "initialize",
		signature = signature("blackboost.regr"),
		def = function(.Object, data, formula, train.fct.pars=list(), predict.fct.pars=list()) {
			train.fct <- "blackboost"
			predict.fct <- "predict"
			
			
			.Object <- callNextMethod(.Object, learner.name="blackboost", learner.pack="mboost",
					learner.model.class="blackboost", learner.model.S4 = FALSE,
					train.fct=train.fct, train.fct.pars=train.fct.pars,
					predict.fct=predict.fct, predict.fct.pars=predict.fct.pars,
					data = data, formula=formula)
			return(.Object)
		}
)





