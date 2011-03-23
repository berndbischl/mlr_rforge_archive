#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include trainLearner.R
roxygen()
#' @include predictLearner.R
roxygen()
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.JRip", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.JRip"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = TRUE,
					numerics = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)
      
      par.set = makeParameterSet(
        makeIntegerLearnerParameter(id="F", default=3L, lower=2L),
        makeNumericLearnerParameter(id="N", default=2, lower=0),
        makeIntegerLearnerParameter(id="O", default=2L, lower=1L),
        makeLogicalLearnerParameter(id="E", default=FALSE),
        makeLogicalLearnerParameter(id="P", default=FALSE)
      )      
			callNextMethod(.Object, pack="RWeka", desc=desc, par.set=par.set)
		}
)

#' @rdname trainLearner


setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.JRip", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			ctrl = Weka_control(..., S=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
			JRip(f, data=get.data(.task, .subset), control=ctrl)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.JRip", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, prob="prob", "class")
			predict(.model@learner.model, newdata=.newdata, type=.type, ...)
		}
)