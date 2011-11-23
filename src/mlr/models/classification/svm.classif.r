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
		"classif.svm", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.svm"),
		def = function(.Object) {
      par.set = makeParamSet(
        makeDiscreteLearnerParam(id="type", default="C-classification", values=c("C-classification", "nu-classification")),
        makeNumericLearnerParam(id="cost",  default=1, lower=0, requires=expression(type=="C-classification")),
        makeNumericLearnerParam(id="nu", default=0.5, requires=expression(type=="nu-classification")),
        makeDiscreteLearnerParam(id="kernel", default="radial", values=c("linear", "polynomial", "radial", "sigmoid")),
        makeIntegerLearnerParam(id="degree", default=3L, lower=1L, requires=expression(kernel=="polynomial")),
        makeNumericLearnerParam(id="coef0", default=0, requires=expression(kernel=="polynomial" || kernel=="sigmoid")),
        makeNumericLearnerParam(id="gamma", lower=0, requires=expression(kernel!="linear")),
        makeNumericLearnerParam(id="tolerance", default=0.001, lower=0),
        makeLogicalLearnerParam(id="shrinking", default=TRUE),
        makeNumericLearnerParam(id="cachesize", default=40L)
      )
      
      .Object = callNextMethod(.Object, pack="e1071", par.set=par.set)
      
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE
      )
      
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.svm", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			svm(f, data=getData(.task, .subset), probability=.learner@predict.type == "prob", ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.svm", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			if(.learner@predict.type == "response") {
				p = predict(.model@learner.model, newdata=.newdata, ...)
			} else {
				p = predict(.model@learner.model, newdata=.newdata, probability=TRUE, ...)
				p = attr(p, "probabilities")
			}
			return(p)
		}
)	


