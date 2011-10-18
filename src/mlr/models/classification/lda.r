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
		"classif.lda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.lda"),
		def = function(.Object) {
			par.set = makeParameterSet(
					makeDiscreteLearnerParameter(id="method", default="moment", vals=c("moment", "mle", "mve", "t")),
					makeNumericLearnerParameter(id="nu", lower=2, requires=expression(method=="t")),
          makeNumericLearnerParameter(id="tol", default=1.0e-4, lower=0)
      )
			.Object = .Object = callNextMethod(.Object, pack="MASS", par.set=par.set)
      setProperties(.Object, 
        twoclass=TRUE, 
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
				.learner="classif.lda", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			lda(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.lda", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			p <- predict(.model@learner.model, newdata=.newdata, ...)
			if(.learner@predict.type == "response")
				return(p$class)
			else
				return(p$posterior)
		}
)	
