#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.classif.r
roxygen()



setClass(
		"classif.PART", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.PART"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = TRUE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)

      par.set = list(
        makeNumericLearnerParameter(id="C", default=0.25, lower=0),
        makeIntegerLearnerParameter(id="M", default=2L, lower=1L),
        makeLogicalLearnerParameter(id="R"),
        makeIntegerLearnerParameter(id="N", default=3L, lower=2L),
        makeLogicalLearnerParameter(id="B"),
        makeLogicalLearnerParameter(id="U"),
        makeLogicalLearnerParameter(id="J")
      )      
			callNextMethod(.Object, pack="RWeka", desc=desc, par.set=par.set)
		}
)

#' @rdname train.learner


setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.PART", 
				.task="classif.task", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			ctrl = Weka_control(..., Q=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
			PART(f, data=get.data(.task, .subset), control=ctrl)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.PART", 
				.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, prob="prob", "class")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)