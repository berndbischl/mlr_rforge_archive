#' @include learnerR.r
roxygen()

setClass(
		"regr.randomForest", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.randomForest"),
		def = function(.Object) {
			
			desc = c(
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					weights = FALSE
			)
			
			callNextMethod(.Object, pack="randomForest", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.randomForest", 
				.task="regr.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
			randomForest(f, data=get.data(.task, .subset, .vars), ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.randomForest", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model["learner.model"], newdata=.newdata, ...)
		}
)	









