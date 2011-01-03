#' @include learnerR.r
roxygen()
#' @include task.regr.r
roxygen()

setClass(
		"regr.mars", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.mars"),
		def = function(.Object) {
			
			desc = c(
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					weights = FALSE
			)
			
			callNextMethod(.Object, pack="mda", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.mars", 
				.task="regr.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			d = get.data(.task, .subset, with.target=FALSE)
			mars(x = as.matrix(d), y = .task["targets"][.subset], ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "regr.mars", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model["learner.model"], newdata=.newdata)
		}
)



