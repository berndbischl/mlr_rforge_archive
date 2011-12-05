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
      par.set = makeParamSet(
        makeIntegerLearnerParam(id="ntree", default=500L, lower=1L),
        makeIntegerLearnerParam(id="mtry", lower=1L),
        makeLogicalLearnerParam(id="replace", default=TRUE),
        makeIntegerLearnerParam(id="sampsize", lower=1L),
        makeIntegerLearnerParam(id="nodesize", default=1L, lower=1L),
        makeIntegerLearnerParam(id="maxnodes", lower=1L),
        makeLogicalLearnerParam(id="importance", default=FALSE),
        makeLogicalLearnerParam(id="localImp", default=FALSE),
        makeLogicalLearnerParam(id="keep.inbag", default=FALSE)
      )
      
			.Object = callNextMethod(.Object, pack="randomForest", par.set=par.set)

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
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model@learner.model, newdata=.newdata, ...)
		}
)	









