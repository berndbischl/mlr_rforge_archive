#' @include learnerR.r
roxygen()

setClass(
		"regr.rpart", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.rpart"),
		def = function(.Object) {
			
			desc = c(
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					weights = TRUE
			)
			callNextMethod(.Object, pack="rpart",	desc=desc, )
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.rpart", 
				.task="regr.task", .subset="integer", .vars="character"
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
			rpart(f, data=get.data(.task, .subset, .vars), weights=.weights, ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.rpart", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model["learner.model"], newdata=.newdata, ...)
		}
)	


