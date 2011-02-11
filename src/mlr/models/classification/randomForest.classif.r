#' @include learnerR.r
roxygen()
#' @include WrappedModel.R
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.randomForest", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.randomForest"),
		def = function(.Object) {
			
			desc = c(
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = TRUE,
					missings = FALSE,
					doubles = TRUE,
					factors = TRUE,
					prob = TRUE,
					decision = FALSE,
					weights = FALSE,
					costs = FALSE
			)
			
			par.set = makeParameterSet(
					makeIntegerLearnerParameter(id="ntree", default=500L, lower=1L),
          makeIntegerLearnerParameter(id="mtry", lower=1L),
					makeLogicalLearnerParameter(id="replace", default=TRUE),
          makeIntegerLearnerParameter(id="sampsize", lower=1L),
          makeIntegerLearnerParameter(id="nodesize", default=1L, lower=1L),
          makeIntegerLearnerParameter(id="maxnodes", lower=1L),
          makeLogicalLearnerParameter(id="importance", default=FALSE, flags=list(optimize=FALSE)),
          makeLogicalLearnerParameter(id="localImp", default=FALSE, flags=list(optimize=FALSE)),
          makeLogicalLearnerParameter(id="norm.votes", default=TRUE, flags=list(optimize=FALSE)),
          makeLogicalLearnerParameter(id="keep.inbag", default=FALSE, flags=list(optimize=FALSE))
			)

      callNextMethod(.Object, pack="randomForest", desc=desc, par.set=par.set)
		}
)

#todo: randomforest crashes, when 1 feature is in data and has 0 variance, this 
# happens in forward search! also fix regr.
#' @rdname train.learner

setMethod(
		f = "train.learner",
		signature = signature(
				.learner="classif.randomForest", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			randomForest(f, data=get.data(.task, .subset), ...)
		}
)

#' @rdname pred.learner

setMethod(
		f = "pred.learner",
		signature = signature(
				.learner = "classif.randomForest", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "prob")
			predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
		}
)	









