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
			
			desc = new("learner.desc.regr",
					missings = TRUE,
					numerics = TRUE,
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
			f = as.formula(paste(.targetvar, "~."))
			rpart(f, data=.data, weights=.weights, ...)
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


