#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


setClass(
		"classif.llr", 
		contains = c("rlearner.classif")
)

setMethod(
		f = "initialize",
		signature = signature("classif.llr"),
		def = function(.Object) {
			
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)			
			
			callNextMethod(.Object, pack="locClass", props=desc)
		}
)

#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.llr", 
				.task="classif.task", .subset="integer", .vars="character" 
		),
		
		def = function(.learner, .task, .subset, .vars,  ...) {
			f = .task["formula"]
			llr(f, data=.task["data"][.subset, .vars], ...)
		}
)

#' @rdname pred.learner


setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.llr", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p <- predict(.model["learner.model"], newdata=.newdata, ...)
			if(.type=="response")
				return(p$class)
			else
				return(p$posterior)
		}
)