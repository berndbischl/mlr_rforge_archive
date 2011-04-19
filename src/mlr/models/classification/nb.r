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
		"classif.naiveBayes", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.naiveBayes"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeNumericLearnerParameter(id="laplace", default=0, lower=0)
        #makeNumericLearnerParameter(id="threshold", default=0.001, lower=0)
      )
      
			.Object = callNextMethod(.Object, pack="e1071", par.set=par.set)

      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        missings = TRUE,
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
				.learner="classif.naiveBayes", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			naiveBayes(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.naiveBayes", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type <- ifelse(.type=="response", "class", "raw")
			predict(.model@learner.model, newdata=.newdata, type=.type, ...)
		}
)	



