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
					makeDiscreteLearnerParameter(id="method", default="moment", vals=c("moment", "mle", "mve", "t")),
					makeNumericLearnerParameter(id="nu", lower=2, requires=expression(method=="t")),
          makeNumericLearnerParameter(id="tol", default=1.0e-4, lower=0)
      )
			
			callNextMethod(.Object, pack="MASS", desc=desc, par.set=par.set)
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
			f = .task["formula"]
			lda(f, data=get.data(.task, .subset), ...)
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.lda", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			p <- predict(.model["learner.model"], newdata=.newdata, ...)
			if(.type=="response")
				return(p$class)
			else
				return(p$posterior)
		}
)	
