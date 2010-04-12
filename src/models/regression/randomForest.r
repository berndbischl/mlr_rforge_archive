#' @include wrapped.learner.regr.r
roxygen()

setClass(
		"regr.randomForest", 
		contains = c("wrapped.learner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.randomForest"),
		def = function(.Object, parset) {
			
			desc = new("regr.props",
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.weights = FALSE
			)
			
			callNextMethod(.Object, learner.name="randomForest", learner.pack="randomForest", learner.props=desc, parset=parset)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="regr.randomForest", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="missing", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			randomForest(f, data=.data, ...)
		}
)

#' @rdname predict.learner

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "regr.randomForest", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			predict(.wrapped.model["learner.model"], newdata=.newdata, ...)
		}
)	









