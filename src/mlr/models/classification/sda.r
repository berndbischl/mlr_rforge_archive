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
		"classif.sda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.sda"),
		def = function(.Object) {
      par.set = list (
        makeLogicalLearnerParam(id="diagonal", default=FALSE)
      )
      
			.Object = callNextMethod(.Object, pack="sda", par.set=par.set)
      
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        prob = TRUE
      )
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.sda", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			d = getData(.task, .subset, target.extra=TRUE)
			sda(Xtrain = as.matrix(d$data), L = d$target, ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.sda", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			p = predict(.model@learner.model, as.matrix(.newdata))
			if(.learner@predict.type == "response")
				return(p$class)
			else
				return(p$posterior)
		}
)



