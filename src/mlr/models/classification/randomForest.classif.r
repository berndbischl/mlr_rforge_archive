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
					numerics = TRUE,
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
          makeNumericVectorLearnerParameter(id="classwt", lower=0),
          makeNumericVectorLearnerParameter(id="cutoff", lower=0, upper=1),
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
#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.randomForest", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset, classwt=NULL, cutoff, ...) {
      f = .task["formula"]
      levs = getClassLevels(.task)
      n = length(levs)
      if (missing(cutoff))
        cutoff = rep(1/n, n)
      if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt))) 
        names(classwt) = levs
      if (is.numeric(cutoff) && length(cutoff) == n && is.null(names(cutoff))) 
        names(cutoff) = levs
			randomForest(f, data=get.data(.task, .subset), classwt=classwt, cutoff=cutoff, ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.randomForest", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "response", "prob")
			predict(.model@learner.model, newdata=.newdata, type=.type, ...)
		}
)	









