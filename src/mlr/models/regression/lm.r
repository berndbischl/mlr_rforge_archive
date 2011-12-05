#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()


setClass(
		"regr.lm", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.lm"),
		def = function(.Object) {
			.Object = callNextMethod(.Object, pack="stats")
      
      setProperties(.Object,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        se = TRUE,
        weights = TRUE
      )
    }
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.lm", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, ...) {
			f = getFormula(.task)
      d = getData(.task, .subset)
      if (.task@desc@has.weights) {
        # strange bug in lm concerning weights
        do.call(lm, list(f, data=d, weights=.task@weights[.subset]))
      }else  
        lm(f, data=d, ...)
    }
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.lm", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
      if(.learner@predict.type == "response") {
        predict(.model@learner.model, newdata=.newdata, se.fit=FALSE, ...)
      } else {
        p = predict(.model@learner.model, newdata=.newdata, se.fit=FALSE, ...)
        cbind(p$fit, p$se.fit)
      }
		}
)	





