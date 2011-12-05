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
      par.set = makeParamSet(
        makeNumericLearnerParam(id="degree", default=1, lower=1),
        makeNumericLearnerParam(id="penalty")
      )
			
      .Object = callNextMethod(.Object, pack="earth", par.set=par.set)
    
      setProperties(.Object,
        missings = FALSE,
        numerics = TRUE,
        factors = TRUE,
        se.fit = FALSE,
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
      #x = getData(.task, .subset, target.extra=TRUE)
			#earth(x$data, x$target, ...)
      f = getFormula(.task)
      args = list(f, data=getData(.task, .subset))
      args = c(args, list(...))
      do.call("earth", args)
    }
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.earth", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model@learner.model, newdata=.newdata)[,1]
		}
)