#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.icr", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.icr"),
		def = function(.Object) {
			
			setProperties(.Object,
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					weights = FALSE
			)
			
			.Object = callNextMethod(.Object, pack="caret")
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.icr", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			icr(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.icr", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model@learner.model, newdata=.newdata)
		}
)



