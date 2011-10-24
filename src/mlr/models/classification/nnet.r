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
			par.set = makeParameterSet(
        makeIntegerLearnerParameter(id="size", default=3L, lower=0L, pass.default=TRUE),
        makeIntegerLearnerParameter(id="maxit", default=100L, lower=1L),
        # nnet seems to set these manually and hard for classification.....
#        makeLogicalLearnerParameter(id="linout", default=FALSE, requires=expression(entropy==FALSE && softmax==FALSE && censored==FALSE)),
#        makeLogicalLearnerParameter(id="entropy", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && censored==FALSE)),
#        makeLogicalLearnerParameter(id="softmax", default=FALSE, requires=expression(entropy==FALSE && linout==FALSE && censored==FALSE)),
#        makeLogicalLearnerParameter(id="censored", default=FALSE, requires=expression(linout==FALSE && softmax==FALSE && entropy==FALSE)),
        makeLogicalLearnerParameter(id="skip", default=FALSE),
        makeNumericLearnerParameter(id="rang", default=0.7),
        makeNumericLearnerParameter(id="decay", default=0),
        makeLogicalLearnerParameter(id="Hess", default=FALSE),
        makeLogicalLearnerParameter(id="trace", default=TRUE),
        makeIntegerLearnerParameter(id="MaxNWts", default=1000L),
        makeNumericLearnerParameter(id="abstoll", default=1.0e-4),
        makeNumericLearnerParameter(id="reltoll", default=1.0e-8)
      )
      			
			.Object = callNextMethod(.Object, pack="nnet", par.set=par.set)
    
      setProperties(.Object, 
        twoclass = TRUE,
        multiclass = TRUE,
        numerics = TRUE,
        factors = TRUE,
        prob = TRUE,
        weights = TRUE
      )
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
			f = getFormula(.task)
      if (.task@desc@has.weights)
        nnet(f, data=getData(.task, .subset), weights=.task@weights[.subset], ...)
      else  
        nnet(f, data=getData(.task, .subset), ...)			
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.nnet", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = switch(.learner@predict.type, response="class", prob="raw")
			p = predict(.model@learner.model, newdata=.newdata, type=type, ...)
			if (type == "class")
				return(as.factor(p))
			else {
				if (length(.model@task.desc@class.levels) == 2) {
          y <- cbind(p, 1-p) 
					colnames(y) = .model@task.desc@class.levels
					return(y)
				} else
					return(p)	
			}
		}
)	
