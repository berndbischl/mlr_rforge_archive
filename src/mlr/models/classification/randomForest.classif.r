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
			par.set = makeParamSet(
					makeIntegerLearnerParam(id="ntree", default=500L, lower=1L),
          makeIntegerLearnerParam(id="mtry", lower=1L),
					makeLogicalLearnerParam(id="replace", default=TRUE),
          makeNumericVectorLearnerParam(id="classwt", lower=0),
          makeNumericVectorLearnerParam(id="cutoff", lower=0, upper=1),
          makeIntegerLearnerParam(id="sampsize", lower=1L),
          makeIntegerLearnerParam(id="nodesize", default=1L, lower=1L),
          makeIntegerLearnerParam(id="maxnodes", lower=1L),
          makeLogicalLearnerParam(id="importance", default=FALSE),
          makeLogicalLearnerParam(id="localImp", default=FALSE),
          makeLogicalLearnerParam(id="norm.votes", default=TRUE),
          makeLogicalLearnerParam(id="keep.inbag", default=FALSE)
			)

      .Object = callNextMethod(.Object, pack="randomForest", par.set=par.set)
    
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE
      )
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
      f = getFormula(.task)
      levs = .task@desc@class.levels
      n = length(levs)
      if (missing(cutoff))
        cutoff = rep(1/n, n)
      if (!missing(classwt) && is.numeric(classwt) && length(classwt) == n && is.null(names(classwt))) 
        names(classwt) = levs
      if (is.numeric(cutoff) && length(cutoff) == n && is.null(names(cutoff))) 
        names(cutoff) = levs
			randomForest(f, data=getData(.task, .subset), classwt=classwt, cutoff=cutoff, ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.randomForest", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = ifelse(.learner@predict.type=="response", "response", "prob")
			predict(.model@learner.model, newdata=.newdata, type=type, ...)
		}
)	









