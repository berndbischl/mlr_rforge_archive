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
      par.set = makeParamSet(
        makeIntegerLearnerParam(id="minsplit", default=20L, lower=1L),
        makeIntegerLearnerParam(id="minbucket", lower=1L),
        makeNumericLearnerParam(id="cp", default=0.01, lower=0, upper=1),
        makeIntegerLearnerParam(id="maxcompete", default=4L, lower=0L),
        makeIntegerLearnerParam(id="maxsurrogate", default=5L, lower=0L),
        makeDiscreteLearnerParam(id="usesurrogate", default=2L, values=0:2),
        makeDiscreteLearnerParam(id="surrogatestyle", default=0L, values=0:1),
        # we use 30 as upper limit, see docs of rpart.control
        makeIntegerLearnerParam(id="maxdepth", default=30L, lower=1L, upper=30L)
      )
      
			.Object = callNextMethod(.Object, pack="rpart", par.set=par.set)
      
      setProperties(.Object,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        se = FALSE,
        weights = TRUE
      )
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
      f = getFormula(.task)
      if (.task@desc@has.weights)
        rpart(f, data=getData(.task, .subset), weights=.task@weights[.subset], ...)
      else  
        rpart(f, data=getData(.task, .subset), ...)
    }
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "regr.rpart", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			predict(.model@learner.model, newdata=.newdata, ...)
		}
)	


