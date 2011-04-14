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

# checked props

setClass(
		"classif.J48", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.J48"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeLogicalLearnerParameter(id="U"),
        makeLogicalLearnerParameter(id="O"),
        makeNumericLearnerParameter(id="C", default=0.25, lower=0),
        makeIntegerLearnerParameter(id="M", default=2L, lower=1L),
        makeLogicalLearnerParameter(id="R"),
        makeIntegerLearnerParameter(id="N", default=3L, lower=2L),
        makeLogicalLearnerParameter(id="B"),
        makeLogicalLearnerParameter(id="S"),
        makeLogicalLearnerParameter(id="L"),
        makeLogicalLearnerParameter(id="A"),
        makeLogicalLearnerParameter(id="J")
      )      
		
      .Object = callNextMethod(.Object, pack="RWeka", par.set=par.set)
      
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE
      )
    }
)

#' @rdname trainLearner


setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.J48", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			ctrl = Weka_control(..., Q=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
			J48(f, data=get.data(.task, .subset), control=ctrl)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.J48", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, prob="prob", "class")
			predict(.model@learner.model, newdata=.newdata, type=.type, ...)
		}
)	





