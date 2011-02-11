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
		"classif.nnet", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.nnet"),
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
					weights = TRUE,
					costs = FALSE
			)

			par.set = makeParameterSet(
        makeIntegerLearnerParameter(id="size", default=3L, lower=0L, flags=list(pass.default=TRUE)),
        makeIntegerLearnerParameter(id="maxit", default=100L, lower=1L),
        # nnet seems to set these manually and hard for classification.....
#        makeLogicalLearnerParameter(id="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#        makeLogicalLearnerParameter(id="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#        makeLogicalLearnerParameter(id="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#        makeLogicalLearnerParameter(id="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
        makeLogicalLearnerParameter(id="skip", default=FALSE),
        makeNumericLearnerParameter(id="rang", default=0.7),
        makeNumericLearnerParameter(id="decay", default=0),
        makeLogicalLearnerParameter(id="Hess", default=FALSE, flags=list(optimize=FALSE)),
        makeLogicalLearnerParameter(id="trace", default=TRUE, flags=list(optimize=FALSE)),
        makeIntegerLearnerParameter(id="MaxNWts", default=1000L),
        makeNumericLearnerParameter(id="abstoll", default=1.0e-4),
        makeNumericLearnerParameter(id="reltoll", default=1.0e-8)
      )
      			
			callNextMethod(.Object, pack="nnet", desc=desc, par.set=par.set)
		}
)

#' @rdname trainLearner

setMethod(
		f = "trainLearner",
		signature = signature(
				.learner="classif.nnet", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = .task["formula"]
      if (.task["has.weights"])
        nnet(f, data=get.data(.task, .subset), weights=.task["weights"][.subset], ...)
      else  
        nnet(f, data=get.data(.task, .subset), ...)			
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.nnet", 
				.model = "WrappedModel", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.learner, .model, .newdata, .type, ...) {
			.type = switch(.type, response="class", prob="raw")
			p = predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
			if (.type == "class")
				return(as.factor(p))
			else {
				if (.model["class.nr"] == 2) {
          y <- cbind(p, 1-p) 
					colnames(y) = .model["class.levels"]
					return(y)
				} else
					return(p)	
			}
		}
)	
