#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.mars", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.mars"),
		def = function(.Object) {
			
			setProperties(.Object,
					missings = FALSE,
					numerics = TRUE,
					factors = FALSE,
					weights = FALSE
			)
			
			.Object = callNextMethod(.Object, pack="mda")
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.mars", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			d = getData(.task, .subset, target.extra=TRUE)
			mars(x = as.matrix(d$data), y = d$target, ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.mars", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model@learner.model, newdata=.newdata)
		}
)



