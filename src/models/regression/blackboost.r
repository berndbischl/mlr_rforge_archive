#' @include wrapped.learner.regr.r
roxygen()

setClass(
		"blackboost.regr", 
		contains = c("wrapped.learner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("blackboost.regr"),
		def = function(.Object, parset) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = TRUE
			)
			
			callNextMethod(.Object, learner.name="blackboost", learner.pack="mboost", learner.props=desc, parset=parset)
		}
)

#' @rdname train.learner

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

#' @rdname predict.learner

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





