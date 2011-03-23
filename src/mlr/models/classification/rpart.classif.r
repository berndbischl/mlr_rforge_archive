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
# todo: transform arbitrary cost matrices to 0 digaonal, also do this for other classifiers and have option to not take cost matrix form task

setClass(
		"classif.rpart", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.rpart"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = TRUE,
					numerics = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = TRUE,
					costs = TRUE
			)
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
			
			callNextMethod(.Object, pack="rpart", desc=desc, par.set=par.set)
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
      f = .task["formula"]
      d = get.data(.task, .subset)
      if (.task["has.costs"]) {
        cm = .task["costs"]
        # probably better to reorder the row/cols so they correspond with levels in d$target
        levs = levels(d[, .task@desc@target]) 
        cm = cm[levs, levs]
        if (.task["has.weights"])
          rpart(f, data=d, weights=.task["weights"][.subset], parms=list(loss=cm), ...)
        else 
          rpart(f, data=d, parms=list(loss=cm), ...)
      } else
      if (.task["has.weights"])
        rpart(f, data=d, weights=.task["weights"][.subset], ...)
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
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, prob="prob", "class")
			predict(.model@learner.model, newdata=.newdata, type=.type, ...)
		}
)	





