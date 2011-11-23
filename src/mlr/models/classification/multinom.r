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
#' @include ClassifTask.R
roxygen()


setClass(
		"classif.multinom", 
		contains = c("rlearner.classif")
)


setMethod(
		f = "initialize",
		signature = signature("classif.multinom"),
		def = function(.Object) {
      par.set = makeParamSet(
        makeLogicalLearnerParam(id="Hess", default=FALSE),
        makeDiscreteLearnerParam(id="summ", default=0L, values=0:3),
        makeLogicalLearnerParam(id="censored", default=FALSE),
        makeLogicalLearnerParam(id="model", default=FALSE),
        makeIntegerLearnerParam(id="maxit", default=100L, lower=1L),
        makeNumericLearnerParam(id="rang", default=0.7),
        makeNumericLearnerParam(id="decay", default=0),
        makeLogicalLearnerParam(id="trace", default=TRUE),
        makeNumericLearnerParam(id="abstoll", default=1.0e-4),
        makeNumericLearnerParam(id="reltoll", default=1.0e-8)
      )
      
      .Object = callNextMethod(.Object, pack="nnet", par.set=par.set)
      
      #checked:
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
				.learner="classif.multinom", 
				.task="ClassifTask", .subset="integer" 
		),
		
		def = function(.learner, .task, .subset,  ...) {
			f = getFormula(.task)
      if (.task@desc@has.weights)
        multinom(f, data=getData(.task, .subset), weights=.task@weights[.subset], ...)
      else  
        multinom(f, data=getData(.task, .subset), ...)			
		}
)

#' @rdname predictLearner

setMethod(
		f = "predictLearner",
		signature = signature(
				.learner = "classif.multinom", 
				.model = "WrappedModel", 
				.newdata = "data.frame" 
		),
		
		def = function(.learner, .model, .newdata, ...) {
			type = ifelse(.learner@predict.type=="response", "class", "probs")
			levs = .model@task.desc@class.levels
			p = predict(.model@learner.model, newdata=.newdata, type=type, ...)
			if (type == "probs" && length(levs)==2) {
				p = matrix(c(1-p, p), ncol=2, byrow=FALSE)
				colnames(p) = levs
			} 
			return(p)
		}
)	







