#' @include learnerR.r
roxygen()

setClass(
		"regr.mars", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.mars"),
		def = function(.Object) {
			
			desc = new("learner.desc.regr",
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					weights = TRUE
			)
			
			callNextMethod(.Object, pack="mda", desc=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="regr.mars", 
				.task="regr.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			targetcol <- which(names(.data) == .targetvar)
			mars(x = as.matrix(.data[,-targetcol]), y = as.vector(.data[,targetcol]), ...)
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



