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
		"classif.llr", 
		contains = c("rlearner.classif")
)

setMethod(
		f = "initialize",
		signature = signature("classif.llr"),
		def = function(.Object) {
			
			
			setProperties(.Object, 
					oneclass = FALSE,
					twoclass = TRUE,
					multiclass = FALSE,
					missings = FALSE,
					numerics = TRUE,
					factors = TRUE,
					prob = TRUE,
					weights = FALSE
			)			
			
			.Object = callNextMethod(.Object, pack="locClass", props=desc)
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.llr", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
			llr(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner


setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.llr", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			p = predict(.model@learner.model, newdata=.newdata, ...)
			if(.learner@predict.type == "response")
				return(p$class)
			else
				return(p$posterior)
		}
)