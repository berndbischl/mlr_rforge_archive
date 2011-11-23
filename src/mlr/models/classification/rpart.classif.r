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


# todo: parms has to be in hyperparamter list

setClass(
		"classif.rpart", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rpart"),
		def = function(.Object) {
			par.set = makeParamSet(
          makeIntegerLearnerParam(id="minsplit", default=20L, lower=1L),
          makeIntegerLearnerParam(id="minbucket", lower=1L),
					makeNumericLearnerParam(id="cp", default=0.01, lower=0, upper=1),
          makeIntegerLearnerParam(id="maxcompete", default=4L, lower=0L),
          makeIntegerLearnerParam(id="maxsurrogate", default=5L, lower=0L),
					makeDiscreteLearnerParam(id="usesurrogate", default=2L, vals=0:2),
					makeDiscreteLearnerParam(id="surrogatestyle", default=0L, vals=0:1),
          # we use 30 as upper limit, see docs of rpart.control
          makeIntegerLearnerParam(id="maxdepth", default=30L, lower=1L, upper=30L)
			)
			
			.Object = callNextMethod(.Object, pack="rpart", par.set=par.set)
      
      .Object = setProperties(.Object,
        twoclass = TRUE,
        multiclass = TRUE,
        missings = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE
      )
		}
)

#' @rdname trainLearner


setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.rpart", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
      f = getFormula(.task)
      d = getData(.task, .subset)
      if (.task@desc@has.weights)
        rpart(f, data=d, weights=.task@weights[.subset], ...)
      else 
        rpart(f, data=d, ...)
    }
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.rpart", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = switch(.learner@predict.type, prob="prob", "class")
			predict(.model@learner.model, newdata=.newdata, type=type, ...)
		}
)	





