#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.rpart", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.rpart"),
		def = function(.Object) {
			
			desc = c(
					missings = TRUE,
					numerics = TRUE,
					factors = TRUE,
					weights = TRUE
			)
			callNextMethod(.Object, pack="rpart",	desc=desc, )
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="regr.rpart", 
				.task="RegrTask", .subset="integer"
		),
		
    def = function(.learner, .task, .subset,  ...) {
      f = .task["formula"]
      if (.task["has.weights"])
        rpart(f, data=get.data(.task, .subset), weights=.task["weights"][.subset], ...)
      else  
        rpart(f, data=get.data(.task, .subset), ...)
    }
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.rpart", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model@learner.model, newdata=.newdata, ...)
		}
)	


