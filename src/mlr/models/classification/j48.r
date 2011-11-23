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
      par.set = makeParamSet(
        makeLogicalLearnerParam(id="U"),
        makeLogicalLearnerParam(id="O"),
        makeNumericLearnerParam(id="C", default=0.25, lower=0),
        makeIntegerLearnerParam(id="M", default=2L, lower=1L),
        makeLogicalLearnerParam(id="R"),
        makeIntegerLearnerParam(id="N", default=3L, lower=2L),
        makeLogicalLearnerParam(id="B"),
        makeLogicalLearnerParam(id="S"),
        makeLogicalLearnerParam(id="L"),
        makeLogicalLearnerParam(id="A"),
        makeLogicalLearnerParam(id="J")
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
			f = getFormula(.task)
			ctrl = Weka_control(..., Q=as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max)))
			J48(f, data=getData(.task, .subset), control=ctrl)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.J48", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = switch(.learner@predict.type, prob="prob", "class")
			predict(.model@learner.model, newdata=.newdata, type=type, ...)
		}
)	





