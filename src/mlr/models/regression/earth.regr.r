#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.earth", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.earth"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeNumericLearnerParameter(id="degree", default=1, lower=1),
        makeNumericLearnerParameter(id="penalty")
      )
			
      .Object = callNextMethod(.Object, pack="earth", par.set=par.set)
    
      setProperties(.Object,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        weights = FALSE
      )
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.earth", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			earth(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.earth", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model@learner.model, newdata=.newdata)[,1]
		}
)