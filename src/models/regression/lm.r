#' @include learnerR.r
roxygen()


setClass(
		"regr.lm", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.lm"),
		def = function(.Object) {
			
			desc = c(
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					weights = TRUE
			)
			
			callNextMethod(.Object, pack="stats", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.lm", 
				.task="regr.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars, ...) {
			f = .task["formula"]
			lm(f, data=get.data(.task, .subset, .vars), weights=.weights, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.lm", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model["learner.model"], newdata=.newdata, ...)
		}
)	





