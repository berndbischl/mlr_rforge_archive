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
		"classif.qda", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.qda"),
		def = function(.Object) {
      par.set = makeParameterSet(
        makeDiscreteLearnerParameter(id="method", default="moment", vals=c("moment", "mle", "mve", "t")),
        makeNumericLearnerParameter(id="nu", default=5 , lower=2, requires=expression(method == "t"))
      )
      
			.Object = callNextMethod(.Object, pack="MASS", par.set=par.set)
    
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
				.learner="classif.qda", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
			qda(f, data=getData(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.qda", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p <- predict(.model@learner.model, newdata=.newdata, ...)
			if(.type=="response")
				return(p$class)
			else
				return(p$posterior)
		}
)	


