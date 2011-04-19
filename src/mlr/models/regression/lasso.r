#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()


setClass(
		"regr.lasso", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.lasso"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeNumericLearnerParameter(id="lambda1", default=0, lower=0)
      )
      .Object = callNextMethod(.Object, pack="penalized", par.set=par.set)
    
      setProperties(.Object,
        missings = TRUE,
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
				.learner="regr.lasso", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, ...) {
			f = .task["formula"]
			penalized(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.lasso", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			m <- .model@learner.model
			.newdata[, .model@desc@target] <- 0
			predict(m, data=.newdata,  ...)[,"mu"]
		}
)	




