#' @include learnerR.r
roxygen()
#' @include RegrTask.R
roxygen()

setClass(
		"regr.randomForest", 
		contains = c("rlearner.regr")
)


setMethod(
		f = "initialize",
		signature = signature("regr.randomForest"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeIntegerLearnerParameter(id="ntree", default=500L, lower=1L),
        makeIntegerLearnerParameter(id="mtry", lower=1L),
        makeLogicalLearnerParameter(id="replace", default=TRUE),
        makeIntegerLearnerParameter(id="sampsize", lower=1L),
        makeIntegerLearnerParameter(id="nodesize", default=1L, lower=1L),
        makeIntegerLearnerParameter(id="maxnodes", lower=1L),
        makeLogicalLearnerParameter(id="importance", default=FALSE),
        makeLogicalLearnerParameter(id="localImp", default=FALSE),
        makeLogicalLearnerParameter(id="keep.inbag", default=FALSE)
      )
      
			.Object = callNextMethod(.Object, pack="randomForest", par.set=par.set)

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
				.learner="regr.randomForest", 
				.task="RegrTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			randomForest(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.randomForest", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model@learner.model, newdata=.newdata, ...)
		}
)	









