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
      par.set = makeParameterSet(
        makeIntegerLearnerParameter(id="minsplit", default=20L, lower=1L),
        makeIntegerLearnerParameter(id="minbucket", lower=1L),
        makeNumericLearnerParameter(id="cp", default=0.01, lower=0, upper=1),
        makeIntegerLearnerParameter(id="maxcompete", default=4L, lower=0L, flags=list(optimize=FALSE)),
        makeIntegerLearnerParameter(id="maxsurrogate", default=5L, lower=0L, flags=list(optimize=FALSE)),
        makeDiscreteLearnerParameter(id="usesurrogate", default=2L, vals=0:2),
        makeDiscreteLearnerParameter(id="surrogatestyle", default=0L, vals=0:1),
        # we use 30 as upper limit, see docs of rpart.control
        makeIntegerLearnerParameter(id="maxdepth", default=30L, lower=1L, upper=30L)
      )
      
			.Object = callNextMethod(.Object, pack="rpart",	desc=desc, par.set=par.set)
      
      setProperties(.Object,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
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
      f = .task["formula"]
      if (.task["has.weights"])
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
				.newdata = "data.frame", 
				.type = "missing" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			predict(.model@learner.model, newdata=.newdata, ...)
		}
)	


